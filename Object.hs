-- Object.hs Copyright 2013-2017 John F. Miller
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, MultiParamTypeClasses  #-}
module Object where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Foldable (toList)

import String
import Name
import Object.Array
import Object.Hash (Hash(..))


import qualified Runtime as R
import Err

type Runtime = R.Runtime State Object
type PID = R.PID Object
type Fn = [Object] -> Runtime Object


data Object = Prim !Primitive
            | Process !PID
            | TrueClass
            | FalseClass
            | Nil
            | VFunction {function:: Fn, arity::Arity, fUID::UID}
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
      , uid :: UID
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
      , uid :: UID
      }
  | Module {
        ivars :: Namespace Object
      , instanceOfClass :: PID
      , globalNamespace :: PID
      , localNamespace  :: PID
      , localCache   :: Namespace Fn
      , methods      :: Namespace Fn
      , childModules :: Namespace PID
      , uid :: UID
      }

instance R.StateClass State Object where
  -- markState :: State -> [PID]
  markState 
    Instance {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      } = 
        return 
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : (mark ivars)
          )
  markState 
    Class {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      , superClass
      , modules
      } = 
        return $ 
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : superClass
            : (mark ivars)
          ) ++ modules
  markState 
    Module {
        ivars
      , instanceOfClass
      , globalNamespace
      , localNamespace
      , childModules
      } = 
        return $ 
          (   instanceOfClass
            : globalNamespace
            : localNamespace
            : (mark ivars)
          ) ++ M.elems childModules

mark map = foldMap f map
  where
    f :: Object -> [PID]
    f (Process pid) = [pid]
    f _ = []

vnil :: Object
vnil = Nil

vint :: Integer -> Object
vint = Prim . VInt

vfloat :: Double -> Object
vfloat = Prim . VFloat

verror :: String -> Object
verror = VError . strMsg

vbool :: Bool -> Object
vbool True  = TrueClass
vbool False = FalseClass

getUID :: Object -> Maybe UID
getUID (Object obj) = Just $ uid obj
getUID (Prim (VHash (Hash{hUID})))  = Just hUID
getUID (VFunction{fUID}) = Just fUID
getUID _ = Nothing

instance Show Object where
  show (Prim p) = show p
  show TrueClass = "true"
  show FalseClass = "false"
  show Nil = "nil"
  show (VFunction _ (a,Just b) _) | a == b = "<function: ("++show a++")>"
  show (VFunction _ (a,Just b) _) = "<function: ("++show a++", "++show b++")>"
  show (VFunction _ (a,Nothing)_ ) = "<function: ("++show a++" ...)>"
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



