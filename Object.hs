module Object where

import qualified Data.Text as T
import qualified Data.Map as M

import {-# SOURCE #-} Eval
import {-# SOURCE #-} AST
import Var
import Continuation (ProcessId)
import qualified Continuation as C

type Fn = [Value] -> EvalM Value

data Value =
    VInt Integer
  | VFloat Double
  | VString SapString
  | VNil | VFalse | VTrue
  | VAtom String
  | VFunction Fn Arity
  | VObject Object -- may want to make this stric in Object
  | VError Err

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
  show (VString st) = show $ text st
  show VNil = "nil"
  show VTrue = "true"
  show VFalse = "false"
  show (VAtom a) = ':':a
  show (VFunction _ (a,Just b)) | a == b = "<function: ("++show a++")>"
  show (VFunction _ (a,Just b)) = "<function: ("++show a++", "++show b++")>"
  show (VFunction _ (a,Nothing)) = "<function: ("++show a++" ...)>"
  show (VObject (Object {klass = Class {properName = n}})) = "<Object "++n++">"
  show (VObject (Object {})) = "<Object>"
  show (VObject (Pid (t,_))) = "<PID "++ show t ++">"
  show (VObject (Class  {properName = n})) = "<Class "++n++">"

type Arity = (Int,Maybe Int)

checkArity :: Arity -> Int -> Bool
checkArity (min, Just max) x | (min <= x) && (x <= max) = True
checkArity (min, Nothing)  x | (min <= x) = True
checkArity _ _ = False

data SapString = SapString {encoding :: String, esscapes :: [String], text :: T.Text } deriving Eq --FixMe

type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)

data Object = Pid (ProcessId Message Value)
            | Object { ivars   :: M.Map String Value  -- instance variables
                     , klass   :: Object   -- the class of this instance
		     , modules :: [Object] -- included modules `head` shadows `tail`
		     , process :: Maybe (ProcessId Message Value) -- must be a PID pointing to this object 
		     }
            | Class   {ivars   :: M.Map String Value  -- instance variables
	             , klass   :: Object   -- the class of this instance (typicall Class)
		     , modules :: [Object] -- included modules `head` shadows `tail`
		     , process :: Maybe (ProcessId Message Value) -- must be a PID pointing to this object
		     , super   :: Object   -- the super-class of this class 
		     , cvars :: M.Map String Value     -- instance methods
		     , properName :: String           -- The name in the "global" scope of this class
		                                      -- possibally empty for anonomous classes.
		     }
            | ROOT

data Message =
    Execute Var [Value] --may want ot make this strict in value
  | Search Var  -- check only ivars more to klass w/ search
  | SearchClass Var  -- check only cvars move to super class
  | Retrieve Var  -- when scopped, look only in ivars no graph search
  | Eval Exp 
  | Terminate

type Continuation = C.Continuation Message Value

valToObj :: Value -> IO Object
valToObj (VObject obj) = return obj
valToObj _ = fail "Primitive to Object maping not implimented yet"
