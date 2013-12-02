module AST where

import qualified Data.ByteString as B
import qualified Data.Map as M

data Exp = 
    EVar Var 
  | EInt Integer
  | EFloat Double
  | EString String -- TODO make this smarter
  | EAtom String
  | ENil
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

type Scope = [String]

type Op = String

type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)

data Var = Var {name :: String, scope :: Scope} | Self deriving Show -- TODO: make scope its own thing
  -- TODO: Better Show for Var

data LHS = 
    LVar Var
  | LIndex Exp [Exp] 
  | LCall Exp String
  | LSend Exp String deriving Show

data Value =
    VInt Integer
  | VFloat Double
  | VString SapString
  | VNil
  | VAtom String
  | VFunction --FixMe
  | VPid Pid
  | VObject Object

instance Show Value where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VString st) = show $ bytes st
  show VNil = "nil"
  show (VAtom a) = ':':a

data SapString = SapString {encoding :: String, esscapes :: [String], bytes :: B.ByteString} --FixMe

type Pid = Integer

data Object = Object {vals :: M.Map String Value}
