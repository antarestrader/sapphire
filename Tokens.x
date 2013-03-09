{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$opSym = [\+\-\/\<\>\%\$\^\~\=\*]
@keywords = if|then|else|elsif|class|module|do|while|until|case
@reserveOp = "->" | \= | \? | "::" | ":" 

tokens :-

  $white+   ;
  @keywords                       {\s -> TKeyword s}
  $digit+\.$digit+([eE]$digit+)?  {\s -> TFloat (read s)}
  $digit+                         {\s -> TInt (read s)}
  $alpha+                         {\s -> TVar s}
  \(                              {\s -> TOpen}
  \)                              {\s -> TClose}
  $opSym+                         {\s -> TOperator s}
  \.                              {\s -> TDot}
{

data Token =
  TKeyword String  | 
  TFloat Double    |
  TInt   Integer   | 
  TVar   String    |
  TOpen | TClose   |
  TDot             |
  TOperator String
    deriving (Eq, Show)
}
