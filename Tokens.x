{
-- Tokens.x Copyright 2013, 2014 John F. Miller
module Tokens (
    Token(..)
  , T(..)
  , scanLine
  , scanBlock
  , scanTokens
  )
where

import LineParser
import Lexer
import Prelude hiding (lines)
import Data.Maybe
import Data.Char
import Control.Monad.Error.Class
import Control.Monad.State.Class

}

$digit = 0-9
$alpha = [a-zA-Z]
$opSym = [\+\-\/\<\>\%\$\^\~\=\*\~\&\|]
@ident = [a-zA-Z_][a-zA-Z0-9_\?\!]*
@stringchar = \\\"|[^\"] -- " <- this is here to keep the text formatting intact
@stringchar2 = \\\'|[^\'] -- '
@keywords = self|if|then|else|elsif|class|module|do|while|until|case|when|end|nil|false|true|def|define|lambda

tokens :-

 <0, interp> $white+   ;
 <0, interp> @keywords               {\s -> makeToken $ TKeyword s}
 <0> ^"#" .*                         {\s -> makeToken $ TMeta s}
 <0> "#" .*  ;
 <interp> "#" [^ \} ]*  ;
 <0, interp> ","                     {\s -> makeToken $ TComma}
 <0, interp> "!"                     {\s -> makeToken $ TBang}
 <0, interp> "::"                    {\s -> makeToken $ TScope}
 <0, interp> "="                     {\s -> makeToken $ TAssign}
 <0, interp> "=="                    {\s -> makeToken $ TOperator s}
 <0, interp> "==="                   {\s -> makeToken $ TOperator s}
 <0, interp> "<="                    {\s -> makeToken $ TOperator s}
 <0, interp> ">="                    {\s -> makeToken $ TOperator s}
 <0, interp> "!="                    {\s -> makeToken $ TOperator s}
 <0, interp> "->"                    {\s -> makeToken $ TSend}
 <0, interp> "<-"                    {\s -> makeToken $ TSuper}
 <0, interp> ^@ident\:$              {\s -> makeToken $ TLabel $ init s}
 <0, interp> ^@ident\:[^\:]          {\s -> do
                                              pushChar (last s)
                                              makeToken $ TLabel $ init (init s)
                                     }
 <0, interp> @ident\:/~\:            {\s -> makeToken $ THashAtom $ init s}
 <0, interp> \:@ident\:$             {\s -> makeToken $ TLabel $ tail (init s)}
 <0, interp> \@@ident                {\s -> makeToken $ TIVar  $ tail s}
 <0, interp> \:@ident\:/~\:          {\s -> makeToken $ TLabel $ tail (init s)}
 <0, interp> \:@ident                {\s -> makeToken $ TAtom  $ tail s}
 <0, interp> $digit+\.$digit+([eE][\+\-]?$digit+)?  {\s -> makeToken $ TFloat (read s)}
 <0, interp> $digit+          {\s -> makeToken $ TInt (read s)}
 <0, interp> @ident                  {\s -> makeToken $ TVar s}
 <0, interp> \(                      {\s -> makeToken $ TOpen}
 <0, interp> \)                      {\s -> makeToken $ TClose}
 <0, interp> \[                      {\s -> makeToken $ TBracket}
 <0, interp> \]                      {\s -> makeToken $ TBracketClose}
 <0> \{                              {\s -> makeToken $ TBrace}
 <0> \}                              {\s -> makeToken $ TBraceClose}
 <0, inperp> $opSym+"="              {\s -> makeToken $ TAssignOp $ init s}
 <0, interp> $opSym+                 {\s -> makeToken $ TOperator s}
 <0, interp> \.                      {\s -> makeToken $ TDot}
 <0, interp> \'                      {\_ -> pushMode sqString >> skip}
 <sqString> [^ \\ \' \n]+            {\s -> appendBuffer s >> skip}
 <sqString> \\ \'                    {\_ -> appendBuffer "'" >> skip}
 <sqString> \\ \\                    {\_ -> appendBuffer "\\" >> skip}
 <sqString> \\                       {\_ -> appendBuffer "\\" >> skip}
 <sqString, dqString> \n             {\_ -> throwError "Missing closing quotation mark (').  End of Line found instead"}
 <sqString> \'                       {\_ -> do { popMode; s <- buffer; clearBuffer; (makeToken $ TString s)}}
 <0,interp> \"                       {\_ -> pushMode dqString >> skip}
 <dqString> [^ \\ \" \n \#]+         {\s -> appendBuffer s >> skip }
 <dqString> \"                       {\_ -> do { popMode; s <- buffer; clearBuffer; (makeToken $ TString s)}}
 <dqString> \\ \"                    {\_ -> appendBuffer "\"" >> skip}
 <dqString> \\ [^ 0-9 x ]            {\s -> appendBuffer (esc $ tail s) >> skip} --TODO TLA escapes, \0, control codes
 <dqString> \\ [0-9]+                {\s -> appendBuffer ([chr $ read $ tail s]) >> skip}
 <dqString> \\ x [0-9 a-f A-F]+      {\s -> appendBuffer ([chr $ read $ ('0':(tail s))]) >> skip}
 <dqString> "#{"                     {\_ -> do
                                              pushMode interp
                                              a <-  ( buffer >>= (makeToken . TString))
                                              clearBuffer
                                              b <-  makeToken StartInterp
                                              return (a ++ b) }
 <interp>  \}                        {\_ -> popMode >> makeToken EndInterp}
{

-- action must be String -> Lexer [Token]

scanTokens :: FilePath
           -> Line
           -> Int
           -> Either String [Token]
scanTokens _ (BlankLine _) _ = Right []
scanTokens fp l m = runLexer init loop
  where
    loop :: Lexer [Token]
    loop = do
      m <- gets (head . mode)
      t <- gets input
      case (alexScan t m) of
        AlexEOF -> case block l of
          Nothing -> (makeToken TEnd)
          Just bk -> return [Token (filename bk) (startLine bk) (indent bk) (TBlock bk)]
        AlexError t' -> throwError "Lexer Error" -- todo more info please
        AlexSkip t' _ -> putInput t' >> loop
        AlexToken t' i act -> do
          putInput t'
          ts <- act (matched t i)
          tss <- loop
          return (ts ++ tss)

    matched :: AlexInput -> Int -> String
    matched (_,_,_,s) l = take l s

    init :: LexState
    init = L { mode = [m]
             , input = (offset l,'\n',[],line l)
             , lsLine = lineNo l
             , lsFile = fp
             , _buffer = id
             }

-- | create a token stream for a line including its associated block
scanLine :: FilePath ->Line -> Either String [Token]
scanLine fp l= scanTokens fp l 0

-- | turn a block of Sapphire code into a token stream
scanBlock :: CodeBlock -> Either String [Token]
scanBlock cb = concat `fmap` mapM (scanLine (filename cb)) (lines cb)

}
