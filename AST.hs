module AST where

import Object(Value)
import Var

data Exp = 
    EVar Var 
  | EInt Integer
  | EValue Value -- allows values to be "shoved" values back into expressions
  | EFloat Double
  | EString String
  | EAtom String
  | ENil | EFalse | ETrue
  | OpStr Exp [(Op,Exp)]
  | Index Exp Exp
  | Lambda [String] Exp
  | Define String [String] [Exp]
  | Def String [String] [Exp]
  | Apply Var [Exp]
  | Call Exp String [Exp]
  | Send Exp String [Exp]
  | Assign LHS Exp
  | OpAssign LHS Op Exp
  | If {predicate :: Exp,consequent :: Exp ,alternate :: Maybe Exp}
  | While Exp Exp
  | Until Exp Exp
  | Class Var (Maybe Var) Exp
  | Module Var Exp
  | Block [Exp]  deriving Show

data LHS = 
    LVar Var
  | LIndex Exp [Exp] 
  | LCall Exp String
  | LSend Exp String deriving Show



