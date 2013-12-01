module AST where

data Exp = 
    EVar Var 
  | EInt Integer
  | EFloat Double
  | EString String -- TODO make this smarter
  | EAtom String
  | ENil
  | OpStr Exp [(Op,Exp)]
  | Index Exp Exp
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

type Scope = [String]

type Op = String

data Var = Var {name :: String, scope :: Scope} | Self deriving Show -- TODO: make scope its own thing

data LHS = 
    LVar Var
  | LIndex Exp [Exp] 
  | LCall Exp String
  | LSend Exp String deriving Show
