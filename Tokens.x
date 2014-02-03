{
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

 <0> $white+   ;
 <0> @keywords                       {\s -> makeToken $ TKeyword s}
 <0> ^"#" .*                         {\s -> makeToken $ TMeta s}
 <0> "#" .*  ;
 <0> ","                             {\s -> makeToken $ TComma}
 <0> "::"                            {\s -> makeToken $ TScope}
 <0> "="                             {\s -> makeToken $ TAssign}
 <0> "=="                            {\s -> makeToken $ TOperator s}
 <0> "==="                           {\s -> makeToken $ TOperator s}
 <0> "->"                            {\s -> makeToken $ TSend}
 <0> "<-"                            {\s -> makeToken $ TSuper}
 <0> ^@ident\:$                      {\s -> makeToken $ TLabel s}
 <0> ^@ident\:[^\:]                  {\s -> makeToken $ TLabel s}
 <0> \:@ident\:$                     {\s -> makeToken $ TLabel $ tail s}
 <0> \@@ident                        {\s -> makeToken $ TIVar  $ tail s}
 <0> \:@ident\:/~\:                  {\s -> makeToken $ TLabel $ tail s}
 <0> \:@ident                        {\s -> makeToken $ TAtom  $ tail s}
 <0> $digit+\.$digit+([eE][\+\-]?$digit+)?  {\s -> makeToken $ TFloat (read s)}
 <0> $digit+                         {\s -> makeToken $ TInt (read s)}
 <0> @ident                          {\s -> makeToken $ TVar s}
 <0> \(                              {\s -> makeToken $ TOpen}
 <0> \)                              {\s -> makeToken $ TClose}
 <0> \[                              {\s -> makeToken $ TBracket}
 <0> \]                              {\s -> makeToken $ TBracketClose}
 <0> \{                              {\s -> makeToken $ TBrace}
 <0> \}                              {\s -> makeToken $ TBraceClose}
 <0> $opSym+"="                      {\s -> makeToken $ TAssignOp $ init s}
 <0> $opSym+                         {\s -> makeToken $ TOperator s}
 <0> \.                              {\s -> makeToken $ TDot}
 <0> \"@stringchar*\"                {\s -> makeToken $ TString $ tail $ init s} --TODO Parse string
 <0> \'@stringchar2*\'               {\s -> makeToken $ TString $ tail $ init s}
{

-- action must be String -> Lexer (Maybe Token)

scanTokens :: FilePath
           -> Line
           -> Int
           -> Either String [Token]
scanTokens _ (BlankLine _) _ = Right []
scanTokens fp l m = runLexer init loop
  where
    loop :: Lexer [Token]
    loop = do
      m <- gets mode
      t <- gets input
      case (alexScan t m) of
        AlexEOF -> case block l of
          Nothing -> maybeToList `fmap` makeToken TEnd
          Just bk -> return [Token (filename bk) (startLine bk) (indent bk) (TBlock bk)]
        AlexError t' -> throwError "Lexer Error" -- todo more info please
        AlexSkip t' _ -> putInput t' >> loop
        AlexToken t' i act -> do
          putInput t'
          token <- act (matched t i)
          ts <- loop
          case token of
            Just t -> return (t:ts)
            Nothing -> return ts
    
    matched :: AlexInput -> Int -> String
    matched (_,_,_,s) l = take l s

    init :: LexState
    init = L { mode = m
             , input = (offset l,'\n',[],line l)
             , lsLine = lineNo l
             , lsFile = fp
             }

-- | create a token stream for a line including its associated block
scanLine :: FilePath ->Line -> Either String [Token]
scanLine fp l= scanTokens fp l 0

-- | turn a block of Sapphire code into a token stream
scanBlock :: CodeBlock -> Either String [Token]
scanBlock cb = concat `fmap` mapM (scanLine (filename cb)) (lines cb)

}
