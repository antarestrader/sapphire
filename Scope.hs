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
import  Control.Monad.State hiding (State)
import  Object
import  Name
import  Var
import  AST
import  Parameters

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

class (MonadError Object m, MonadState State m) => Scope m where
  readVar    :: VariableContext -> Name -> m (Maybe (Value m))
  findVar    :: Var -> m (Maybe (Value m))
  setVar     :: VariableContext -> Name -> Object -> m ()
  defMethod  :: Visibility -- ^ Public | Private | Protected
             -> Order -- ^ Append | Prepend | Replace
             -> Name  -- ^ Name of method
             -> Parameter -- ^ the parameters to match
             -> [Exp] -- ^ The actions to execute, one per parameter (presumably block)
             -> m ()
  getMethod  :: Name -> [Object] -> m (Maybe Fn)
  self       :: m (Value m)
  future     :: m PID
  super      :: maybe [Object] -> m Response
  newScope   :: m Response -> m (Value m)
  call       :: Maybe (Value m) -> Name -> [Object] -> m (Value m)
  send       :: PID -> Name -> [Object] -> (Maybe (PID,Name)) -> m ()
  tailCall   :: Maybe (Value m) -> Name -> [Object] -> m Response
  reply      :: Object -> m Response
  spawn      :: Object -> m PID 
  presidenceTable :: m (PrecedenceTable)
  nextUID    :: m UID
