{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$opSym = [\+\-\/\<\>\%\$\^\~\=\*\~\&\|]
@ident = [a-zA-Z_][a-zA-Z0-9_\?\!]*
@keywords = if|then|else|elsif|class|module|do|while|until|case|when|end|nil

tokens :-

  $white+   ;
  @keywords                       {\s -> TKeyword s}
  ","                             {\s -> TComma}
  "::"                            {\s -> TScope}
  "="                             {\s -> TAssign}
  "=="                            {\s -> TOperator s}
  "==="                           {\s -> TOperator s}
  "->"                            {\s -> TSend}
  "<-"                            {\s -> TSuper}
  ^@ident\:$                      {\s -> TLabel s }
  ^@ident\:[^\:]                  {\s -> TLabel s }
  \:@ident\:$                     {\s -> TLabel $ tail s}
  \:@ident\:/~\:                  {\s -> TLabel $ tail s}
  \:@ident                        {\s -> TAtom  $ tail s}
  $digit+\.$digit+([eE][\+\-]?$digit+)?  {\s -> TFloat (read s)}
  $digit+                         {\s -> TInt (read s)}
  @ident                          {\s -> TVar s}
  \(                              {\s -> TOpen}
  \)                              {\s -> TClose}
  \[                              {\s -> TBracket}
  \]                              {\s -> TBracketClose}
  $opSym+                         {\s -> TOperator s}
  "="$opSym+                      {\s -> TAssignOp $ tail s}
  \.                              {\s -> TDot}
{

data Token =
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
  TOperator String
    deriving (Eq, Show)
}
