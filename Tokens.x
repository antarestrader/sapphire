{
module Tokens where

import LineParser
import Text.Printf
import Prelude hiding (lines)

}

%wrapper "posn"


$digit = 0-9
$alpha = [a-zA-Z]
$opSym = [\+\-\/\<\>\%\$\^\~\=\*\~\&\|]
@ident = [a-zA-Z_][a-zA-Z0-9_\?\!]*
@stringchar = \\\"|[^\"] -- " <- this is here to keep the text formattin intact
@keywords = if|then|else|elsif|class|module|do|while|until|case|when|end|nil|def|define

tokens :-

  $white+   ;
  @keywords                       {\(AlexPn _ _ i) s -> (i, TKeyword s)}
  ","                             {\(AlexPn _ _ i) s -> (i, TComma)}
  "::"                            {\(AlexPn _ _ i) s -> (i, TScope)}
  "="                             {\(AlexPn _ _ i) s -> (i, TAssign)}
  "=="                            {\(AlexPn _ _ i) s -> (i, TOperator s)}
  "==="                           {\(AlexPn _ _ i) s -> (i, TOperator s)}
  "->"                            {\(AlexPn _ _ i) s -> (i, TSend)}
  "<-"                            {\(AlexPn _ _ i) s -> (i, TSuper)}
  ^@ident\:$                      {\(AlexPn _ _ i) s -> (i, TLabel s)}
  ^@ident\:[^\:]                  {\(AlexPn _ _ i) s -> (i, TLabel s )}
  \:@ident\:$                     {\(AlexPn _ _ i) s -> (i, TLabel $ tail s)}
  \:@ident\:/~\:                  {\(AlexPn _ _ i) s -> (i, TLabel $ tail s)}
  \:@ident                        {\(AlexPn _ _ i) s -> (i, TAtom  $ tail s)}
  $digit+\.$digit+([eE][\+\-]?$digit+)?  {\(AlexPn _ _ i) s -> (i, TFloat (read s))}
  $digit+                         {\(AlexPn _ _ i) s -> (i, TInt (read s))}
  @ident                          {\(AlexPn _ _ i) s -> (i, TVar s)}
  \(                              {\(AlexPn _ _ i) s -> (i, TOpen)}
  \)                              {\(AlexPn _ _ i) s -> (i, TClose)}
  \[                              {\(AlexPn _ _ i) s -> (i, TBracket)}
  \]                              {\(AlexPn _ _ i) s -> (i, TBracketClose)}
  $opSym+"="                      {\(AlexPn _ _ i) s -> (i, TAssignOp $ init s)}
  $opSym+                         {\(AlexPn _ _ i) s -> (i, TOperator s)}
  \.                              {\(AlexPn _ _ i) s -> (i, TDot)}
  \"@stringchar*\"                 {\(AlexPn _ _ i) s -> (i, TString s)}
{

data T =
  TKeyword String  | 
  TFloat Double    |
  TInt   Integer   | 
  TVar   String    |
  TOpen | TClose   |
  TBracket | TBracketClose |
  TDot | TSend     |
  TScope | TSuper  |
  TAssign | TComma |
  TAssignOp String |
  TAtom  String    |
  TLabel String    |
  TOperator String |
  TString String   |
  TEnd             |
  TBlock CodeBlock
    deriving (Eq, Show)

data Token = Token {tline :: LineNo, toffset::Offset, token::T}

instance Show Token where
  show t = printf "[%s (%i,%i)]" (show $ token t) (tline t) (toffset t)

scanLine :: Line -> [Token]
scanLine l = (map (\(i,t)->Token (lineNo l) (fromIntegral i) t) $ alexScanTokens (line l ++ "\n")) ++ (cb l)
  where
    cb :: Line -> [Token]
    cb Line {block = Nothing} = [Token (lineNo l) (offset l + fromIntegral (length (line l))) TEnd]
    cb Line {block = Just bk} = [Token (startLine bk) (indent bk) (TBlock bk)] 


scanBlock :: CodeBlock -> [Token]
scanBlock = concat . map scanLine . lines

}
