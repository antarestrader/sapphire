module Builtin.Utils where

import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import {-# SOURCE #-} Eval
import Object
import AST
import Context (self, replyM_)
import Object.Graph (lookupIVarsM)
import {-# SOURCE #-} Object.Spawn (spawn)
import Var (simple)

innerValue :: EvalM Value
innerValue = do
  slf <- gets self
  val <- lookupIVarsM "__value" slf
  case val of
    Just v -> return v
    Nothing -> return $ VError "Internal Value at @__value was not found in self"

buildClass :: String -> M.Map String Value -> EvalM Object
buildClass name bootstrap = do
  VObject superClass <- eval ( EVar $ simple "Object")
  VObject clsClass   <- eval ( EVar $ simple "Class")
  let cls = 
        VObject Class
          { ivars = M.empty
          , klass = clsClass
          , modules = []
          , process = Nothing
          , super = superClass
          , cvars = bootstrap
          , properName = name
          }
  pid <- liftIO $ spawn cls
  -- sendM pid $ Eval <<initialization>>  -- no initialization needed at this time
  eval $ Call (EVar $ simple "Object") "setCVar" [EAtom name, EValue $ VObject pid]
  return pid

