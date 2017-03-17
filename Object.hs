-- Object.hs Copyright 2013-2017 John F. Miller
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, MultiParamTypeClasses  #-}
module Object where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
--import Control.Concurrent.STM
--import Control.Concurrent
--import Data.Maybe
import Data.Foldable (toList)

import String
import Array (Array)
--import qualified Array as A
import Hash (Hash)
--import Var
--import qualified Continuation as C

import qualified Runtime as R
import Err

type Runtime = R.Runtime State Object
type PID = R.PID Object
type Fn = [Object] -> Runtime Object
type Arity = (Int,Maybe Int)
type Namespace a = Map String a

data Object = Prim !Primitive
            | Process !PID
            | TrueClass
            | FalseClass
            | Nil
            | VFunction {function:: Fn, arity::Arity}
            | Object State
            | VError !(Err Object)

data Primitive = VInt    !Integer
               | VString !SapString
               | VFloat  !Double
               | VArray !Array
               | VHash !Hash
               | VAtom !String

instance IsString Primitive where
  fromString str = VString $ fromString str

instance IsString Object where
  fromString str = Prim $ fromString str

instance R.Obj Object where
  toObj pid = Process pid

data State = 
    ROOT
  | Instance {
        ivars :: Namespace Object
      , instanceOfClass :: PID
      , globalNamespace :: PID
      , localNamespace  :: PID
      , localCache :: Namespace Fn
      , primitive  :: Maybe Primitive
      }
  | Class {
        ivars :: Namespace Object
      , instanceOfClass :: PID
      , globalNamespace :: PID
      , localNamespace  :: PID
      , localCache  :: Namespace Fn
      , superClass  :: PID
      , methods     :: Namespace Fn
      , methodCache :: Namespace Fn
      , modules     :: [PID]
      }
  | Module {
        ivars :: Namespace Object
      , instanceOfClass :: PID
      , globalNamespace :: PID
      , localNamespace  :: PID
      , localCache   :: Namespace Fn
      , methods      :: Namespace Fn
      , childModules :: Namespace PID
      }

instance R.StateClass State Object where
  -- markState :: State -> [PID]
  markState Instance{ivars, instanceOfClass, globalNamespace, localNamespace} = 
      return (instanceOfClass:globalNamespace:localNamespace:(mark ivars))
  markState Class{ivars, instanceOfClass, globalNamespace, localNamespace, superClass, modules} = 
      return $ (instanceOfClass:globalNamespace:localNamespace:superClass:(mark ivars)) ++ modules
  markState Module{ivars, instanceOfClass, globalNamespace, localNamespace, childModules} = 
      return $ (instanceOfClass:globalNamespace:localNamespace:(mark ivars)) ++ M.elems childModules

mark map = foldMap f map
  where
    f :: Object -> [PID]
    f (Process pid) = [pid]
    f _ = []

checkArity :: Arity -> Int -> Bool
checkArity (min, Just max) x | (min <= x) && (x <= max) = True
checkArity (min, Nothing)  x | (min <= x) = True
checkArity _ _ = False

vnil :: Object
vnil = Nil

verror :: String -> Object
verror = VError . strMsg

vbool :: Bool -> Object
vbool True  = TrueClass
vbool False = FalseClass


instance Show Object where
  show (Prim p) = show p
  show TrueClass = "true"
  show FalseClass = "false"
  show Nil = "nil"
  show (VFunction _ (a,Just b)) | a == b = "<function: ("++show a++")>"
  show (VFunction _ (a,Just b)) = "<function: ("++show a++", "++show b++")>"
  show (VFunction _ (a,Nothing)) = "<function: ("++show a++" ...)>"
  show (Process p) = "<PID "++ show p ++">"
  show (VError e) = show e
  show (Object ROOT) = "ROOT"
  show (Object Instance{}) = "<Instance>"
  show (Object Class{}) = "<Class>"
  show (Object Module{}) = "<Module>"


instance Show Primitive where 
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VString st) = show st
  show (VAtom a) = ':':a
  show (VArray a) = show $ toList a
  show (VHash h) = show $ h

{-
type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)

data Object = Pid Process
            | Object { ivars   :: M.Map String Value  -- ^ instance variables
                     , klass   :: Object   -- ^ the class of this instance
		     , modules :: [Object] -- ^ included modules `head` shadows `tail`
		     , process :: TMVar Process  -- ^ must be a PID pointing to this object
		     }
            | Class   {ivars   :: M.Map String Value  -- ^ instance variables
	             , klass   :: Object   -- ^ the class of this instance (typicall Class)
		     , modules :: [Object] -- ^ included modules `head` shadows `tail`
		     , process :: TMVar Process -- ^ must be a PID pointing to this object
		     , super   :: Object   -- ^ the super-class of this class
		     , cvars :: M.Map String Value     -- ^ instance methods
		     , cmodules :: [Object]            -- ^ included class modules
                     , properName :: String            -- ^ The name in the "global" scope of this class
		                                       --   possibally empty for anonomous classes.
		     }
            | ROOT

instance Eq Object where
  (Pid a) == (Pid b) = a == b
  (Pid _) == _ = False
  ROOT == ROOT = True
  ROOT == _ = False
  _ == (Pid _) = False
  _ == ROOT = False
  a == b = process a == process b


data Message =
    Execute Var [Value] -- ^ may want ot make this strict in value
  | Search SearchIn String
  | SetIVar String Value
  | SetCVar String Value
  | PushModule Value
  | PushCModule Value
  | Eval Exp
  | Initialize Process -- ^ Set process to Pid and call initialization method
  | Terminate

data MethodWithSuper = MWS {runMWS :: EvalM(Maybe MethodWithSuper), mwsValue:: Value}

type Super = EvalM(Maybe MethodWithSuper)

emptySuper :: Super
emptySuper = return Nothing

data SearchIn = IVars | CVars | ObjectGraph | ClassGraph | Methods

data Response = Response Value | ResponseWithSuper MethodWithSuper | NothingFound | Error (Err Value)

responseToValue (Response v) = v
responseToValue (ResponseWithSuper mws) = mwsValue mws
responseToValue (NothingFound) = vnil
responseToValue (Error err) = VError err

type Continuation = C.Continuation Message Response
type Process = C.ProcessId Message Response
type Replier = C.Replier Response
type Responder = C.Responder Object Message Response

thread :: Object -> IO String  -- for debugging purposes only
thread (Pid pid) = return $ show $ fst pid
thread ROOT = return "ROOT"
thread obj = do
  pro <- atomically $ tryTakeTMVar $ process obj
  case pro of
    Nothing -> myThreadId >>= return . show
    Just pid -> return $ show $ fst pid

-}

