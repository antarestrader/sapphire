module Builtin.Module where

import Control.Monad.IO.Class
import Control.Concurrent.STM.TMVar
import Control.Monad.State
import qualified Data.Map as M

import Builtin.Utils
import String
import Err
import Eval
import Object
import Var
import Context
import {-# SOURCE #-} Eval
import {-# SOURCE #-} Object.Spawn (spawn)
import AST
import Control.Monad.Except



moduleClass ::EvalM Object
moduleClass = do
  c <- eval ( EVar $ simple "Class")
  clsClass   <- case c of
    VObject c' -> return c'
    val -> throwError $ Err "SystemError" "the class Class missing" [val]
  tmvar <- liftIO $ newEmptyTMVarIO
  let cls =  Class
          { ivars = M.empty
          , klass = clsClass
          , modules = []
          , process = tmvar
          , super = clsClass
          , cvars = M.empty
          , cmodules = []
          , properName = "Module"
          }
  pid <- spawn cls
  -- sendM pid $ Eval <<initialization>>  -- no initialization needed at this time
  scp <- gets Context.scope
  callT (VObject scp) "setIVar" [VAtom "Module", VObject pid]
  return pid

