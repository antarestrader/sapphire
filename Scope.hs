-- Scope.hs Copyright 2017 John F. Miller
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, MultiParamTypeClasses, FlexibleContexts  #-}
module Scope where

import  Data.Map.Strict (Map)
import  Control.Monad.Except
import  Object
import  Name

data VariableContext = Local
                     | IVar
                     | CVar
                     | Method
                     | Class
                     | Self

data Value m = MV {obj::Object, replace :: (Object -> m ()) }
             | RO {obj::Object}
             | Pointer PID

class (MonadError Object m) => Scope m where
  readVar  :: VariableContext -> Name -> m (Maybe (Value m))
  setVar   :: VariableContext -> Name -> Object -> m ()
  call     :: Maybe (Value m) -> Name -> [Object] -> m (Value m)
  send     :: PID -> Name -> [Object] -> (Object -> m ()) -> m ()
  tailCall :: Maybe (Value m) -> Name -> [Object] -> m ()
  spawn    :: Object -> m Object
  presidenceTable :: m (PrecedenceTable)
