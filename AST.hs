module AST where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

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
  | VNil | VFalse | VTrue
  | VAtom String
  | VFunction ([Value]-> EvalM Value) Arity
  | VPid Pid
  | VObject Object

instance Eq Value where
  (VInt i) == (VInt j) = i == j
  (VFloat f) == (VFloat g) = f == g
  (VString s) == (VString t) = s == t
  VAtom s == VAtom t = s == t
  VNil == VNil = True
  VTrue == VTrue = True
  VFalse == VFalse = True
  _ == _ = False
  
instance Show Value where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VString st) = show $ bytes st
  show VNil = "nil"
  show VTrue = "true"
  show VFalse = "false"
  show (VAtom a) = ':':a
  show (VFunction _ (a,Just b)) | a == b = "<function: ("++show a++")>"
  show (VFunction _ (a,Just b)) = "<function: ("++show a++", "++show b++")>"
  show (VFunction _ (a,Nothing)) = "<function: ("++show a++" ...)>"

type Arity = (Int,Maybe Int)

checkArity :: Arity -> Int -> Bool
checkArity (min, Just max) x | (min <= x) && (x <= max) = True
checkArity (min, Nothing)  x | (min <= x) = True
checkArity _ _ = False

type EvalM a= StateT Context (ErrorT String IO) a

type Context = M.Map String Value

runEvalM :: (EvalM a) -> Context -> IO (Either String (a, Context))
runEvalM e c = runErrorT $ runStateT e c

data SapString = SapString {encoding :: String, esscapes :: [String], bytes :: B.ByteString} deriving Eq --FixMe

type Pid = Integer

data Object = Object {vals :: M.Map String Value}
