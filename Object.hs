module Object where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

type Fn = [Value] -> EvalM Value

type Method = Object -> [Value] -> EvalM Value

data Value =
    VInt Integer
  | VFloat Double
  | VString SapString
  | VNil | VFalse | VTrue
  | VAtom String
  | VFunction Fn Arity
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

data SapString = SapString {encoding :: String, esscapes :: [String], bytes :: B.ByteString} deriving Eq --FixMe

type Pid = Integer

type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)

data Object = Object {ivars :: M.Map String Value, klass :: Pid, methods :: M.Map String Method}

type Context = M.Map String Value

type EvalM a= StateT Context (ErrorT String IO) a

runEvalM :: (EvalM a) -> Context -> IO (Either String (a, Context))
runEvalM e c = runErrorT $ runStateT e c
