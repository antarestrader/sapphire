{-# LANGUAGE 
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , FunctionalDependencies
  , NamedFieldPuns 
  #-}

module Runtime.Runtime where

import Control.Concurrent
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Except
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

import Runtime.Hole
import Runtime.GarbageCollector
import Runtime.PID
import Name

type RunTimeM s obj = StateT (RunTimeState s obj) (ExceptT obj IO)
type ShadowMap obj = Map ThreadId (PID obj)
type Fn st a = Name -> [a] -> Runtime st a a


newtype Runtime st obj a = Runtime {unRuntime :: RunTimeM st obj a}
  deriving (Functor, Applicative, Monad)

class Obj a where
  toObj :: PID a -> a

class StateClass st obj | st -> obj where
  markState :: st -> IO [PID obj]

data RunTimeState st obj = RTS {
    response :: Hole obj
  , error    :: Hole obj
  , state    :: st
  , gc       :: GC obj
  , ourself  :: PID obj
  , shadows  :: ShadowMap obj
  , fn       :: Fn st obj -- this is where eval goes
  }

run :: RunTimeState st obj 
    -> RunTimeM st obj a 
    -> IO (Either obj (a, RunTimeState st obj))
run s a = runExceptT $ runStateT a s

markRTS :: StateClass st obj => RunTimeState st obj -> IO [PID obj]
markRTS rts = ((M.elems (shadows rts)) ++) <$> markState (state rts) 
 
