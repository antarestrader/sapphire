-- Scope.hs Copyright 2017 John F. Miller
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, 
    MultiParamTypeClasses, FlexibleContexts  #-}
module Scope (
    VariableContext(..)
  , Value(..)
  , Scope(..)
  , v , o
  )
where

import  Data.Map.Strict (Map)
import  Control.Monad.Except
import  Object
import  Name
import  Var

data VariableContext = Local
                     | IVar
                     | CVar
                     | Class

data Value m = MV {obj::Object, replace :: (Object -> m ()) }
             | RO {obj::Object}
             | Pointer PID

v :: Object -> Value m
v (Process pid) = Pointer pid
v obj = RO obj

o :: Value m -> Object
o (Pointer pid) = Process pid
o val = obj val

class (MonadError Object m) => Scope m where
  readVar    :: VariableContext -> Name -> m (Maybe (Value m))
  findVar    :: Var -> m (Maybe (Value m))
  setVar     :: VariableContext -> Name -> Object -> m ()
  defMethod  :: Visibility -- ^ Public | Private | Protected
             -> Order -- ^ Append | Prepend | Replace
             -> Name  -- ^ Name of method
             -> ([Object] -> m(Int)) -- ^ Function to parse Params (throw error when not matched)
             -> [m (Value m)] -- The actions to execute
  getMethod  :: Name -> [Object] -> Maybe (m (Value m))
  self       :: m (Value m)
  newScope   :: m a -> m a
  call       :: Maybe (Value m) -> Name -> [Object] -> m (Value m)
  send       :: PID -> Name -> [Object] -> (Object -> m ()) -> m ()
  tailCall   :: Maybe (Value m) -> Name -> [Object] -> m ()
  spawn      :: Object -> m Object
  presidenceTable :: m (PrecedenceTable)
  nextUID    :: m UID
