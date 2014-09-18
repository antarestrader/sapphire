-- Object.hs Copyright 2013, 2014 John F. Miller
{-# LANGUAGE BangPatterns #-}
module Object where

import qualified Data.Map as M

import {-# SOURCE #-} Eval
import {-# SOURCE #-} AST
import String
import Array (Array)
import qualified Array as A
import Hash (Hash)
import Var
import Data.Foldable (toList)
import qualified Continuation as C
import Data.Maybe

-- | The type of function values in sapphire
--   to enable proper tail calls function must reply to the context rather than
--   return a value.
type Fn = [Value] -> EvalM ()

data Value =
    VInt !Integer
  | VFloat !Double
  | VString !SapString
  | VArray !Array
  | VHash !Hash
  | VNil | VFalse | VTrue
  | VAtom !String
  | VFunction{function::Fn, arity::Arity}
  | VObject !Object -- may want to make this strict in Object
  | VError !String

vnil :: Value
vnil = VNil

verror :: String -> Value
verror = VError

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
  show (VString st) = show st
  show VNil = "nil"
  show VTrue = "true"
  show VFalse = "false"
  show (VAtom a) = ':':a
  show (VArray a) = show $ toList a
  show (VHash h) = show $ h
  show (VFunction _ (a,Just b)) | a == b = "<function: ("++show a++")>"
  show (VFunction _ (a,Just b)) = "<function: ("++show a++", "++show b++")>"
  show (VFunction _ (a,Nothing)) = "<function: ("++show a++" ...)>"
  show (VObject (Object {klass = Class {properName = n}})) = "<Object "++n++">"
  show (VObject (Object {})) = "<Object>"
  show (VObject (Pid (t,_))) = "<PID "++ show t ++">"
  show (VObject (Class  {properName = n})) = "<Class "++n++">"
  show (VError err) = "<ERROR: " ++ err ++" >"

type Arity = (Int,Maybe Int)

checkArity :: Arity -> Int -> Bool
checkArity (min, Just max) x | (min <= x) && (x <= max) = True
checkArity (min, Nothing)  x | (min <= x) = True
checkArity _ _ = False

type Precedence = (Int, AssocLR, AssocLR)

data AssocLR = L | R | N deriving (Show,Eq,Ord)

data Object = Pid Process
            | Object { ivars   :: M.Map String Value  -- ^ instance variables
                     , klass   :: Object   -- ^ the class of this instance
		     , modules :: [Object] -- ^ included modules `head` shadows `tail`
		     , process :: Maybe Process  -- ^ must be a PID pointing to this object
		     }
            | Class   {ivars   :: M.Map String Value  -- ^ instance variables
	             , klass   :: Object   -- ^ the class of this instance (typicall Class)
		     , modules :: [Object] -- ^ included modules `head` shadows `tail`
		     , process :: Maybe Process -- ^ must be a PID pointing to this object
		     , super   :: Object   -- ^ the super-class of this class
		     , cvars :: M.Map String Value     -- ^ instance methods
		     , cmodules :: [Object]            -- ^ included class modules
                     , properName :: String            -- ^ The name in the "global" scope of this class
		                                       --   possibally empty for anonomous classes.
		     }
            | ROOT

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


data SearchIn = IVars | CVars | ObjectGraph | ClassGraph

data Response = Response Value | NothingFound | Error String

responseToValue (Response v) = v
responseToValue (NothingFound) = vnil
responseToValue (Error str) = VError "str"

type Continuation = C.Continuation Message Response
type Process = C.ProcessId Message Response
type Replier = C.Replier Response
type Responder = C.Responder Object Message Response

thread :: Object -> String  -- for debugging purposes only
thread (Pid pid) = show $ fst pid
thread ROOT = "ROOT"
thread obj | isJust (process obj) = show $ fst $ fromJust $ process obj
thread _ = "unkonwn"
