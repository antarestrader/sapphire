module Builtin.Utils where

import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import {-# SOURCE #-} Eval
import Object
import AST
import Context (self, replyM_)
import Object.Graph (lookupIVar, MutableValue(..))
import {-# SOURCE #-} Object.Spawn (spawn)
import Var (simple)

innerValue :: EvalM Value
innerValue = do
  val <- lookupIVar "__value"
  case val of
    (MV _ VNil) -> return $ VError "Internal Value at @__value was not found in self"
    (MV _ v) -> return v

updateInnerValue :: Value -> EvalM ()
updateInnerValue val = do
  (MV f _) <- lookupIVar "__value"
  f val

buildClass :: String -> M.Map String Value -> EvalM Object
buildClass name bootstrap = do
  VObject superClass <- eval ( EVar $ simple "Object")
  VObject clsClass   <- eval ( EVar $ simple "Class")
  let cls =  Class
          { ivars = M.empty
          , klass = clsClass
          , modules = []
          , process = Nothing
          , super = superClass
          , cvars = bootstrap
          , cmodules = []
          , properName = name
          }
  pid <- liftIO $ spawn cls
  -- sendM pid $ Eval <<initialization>>  -- no initialization needed at this time
  eval $ Call (EVar $ simple "Object") "setCVar" [EAtom name, EValue $ VObject pid]
  return pid

mkBool :: Bool -> Value
mkBool True = VTrue
mkBool False = VFalse

call :: Value -> String -> [Value] -> EvalM Value
call target msg args = eval $ Call (EValue target) msg args'
  where args' = map EValue args

callT :: Value -> String -> [Value] -> EvalM ()
callT target msg args = evalT $ Call (EValue target) msg args'
  where args' = map EValue args