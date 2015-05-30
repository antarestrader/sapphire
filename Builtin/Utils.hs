module Builtin.Utils where

import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Monad.Except
import Control.Concurrent.STM
import {-# SOURCE #-} Eval
import Object
import AST
import Err
import Context (self, replyM_, sendM, scope)
import Object.Graph (lookupIVar, MutableValue(..))
import {-# SOURCE #-} Object.Spawn (spawn)
import Var (simple)
import String

innerValue :: EvalM Value
innerValue = do
  val <- lookupIVar "__value"
  case val of
    (MV _ VNil) -> return $ VError $ Err "SystemError" "Internal Value at @__value was not found in self" []
    (MV _ v) -> return v

updateInnerValue :: Value -> EvalM ()
updateInnerValue val = do
  (MV f _) <- lookupIVar "__value"
  f val

buildClassEx :: String -> M.Map String Value -> M.Map String Value -> EvalM Object
buildClassEx name bootstrap clsBoot = do
  sc <- eval ( EVar $ simple "Object")
  superClass <-  case sc of
    VObject s -> return s
    val -> throwError $ Err "SystemError" "`Object` not found" [val]
  VObject clsClass   <- eval ( EVar $ simple "Class")
  tmvar <- liftIO $ newEmptyTMVarIO
  let cls =  Class
          { ivars = clsBoot
          , klass = clsClass
          , modules = []
          , process = tmvar
          , super = superClass
          , cvars = bootstrap
          , cmodules = []
          , properName = name
          }
  pid <- spawn cls
  -- sendM pid $ Eval <<initialization>>  -- no initialization needed at this time
  scp <- gets scope
  callT (VObject scp) "setCVar" [VAtom name, VObject pid]
  return pid

includeModule :: Object -> M.Map String Value -> EvalM Object
includeModule obj methods = do
  sc <- eval ( EVar $ simple "Object")
  superClass <-  case sc of
    VObject s -> return s
    val -> throwError $ Err "SystemError" "`Object` not found" [val]
  VObject clsClass   <- eval ( EVar $ simple "Module")
  tmvar <- liftIO $ newEmptyTMVarIO
  let mdl = Class
          { ivars = M.empty
          , klass = clsClass
          , modules = []
          , process = tmvar
          , super = superClass
          , cvars = methods
          , cmodules = []
          , properName = "*"
          }
  case obj of
    (Pid p) -> (sendM p $ PushModule $ VObject mdl) >> return obj
    o -> return o{modules = (mdl:(modules o))}

buildClass :: String -> M.Map String Value -> EvalM Object
buildClass a b = buildClassEx a b M.empty

mkBool :: Bool -> Value
mkBool True = VTrue
mkBool False = VFalse

call :: Value -> String -> [Value] -> EvalM Value
call target msg args = eval $ Call (EValue target) msg args'
  where args' = map EValue args

callT :: Value -> String -> [Value] -> EvalM ()
callT target msg args = evalT $ Call (EValue target) msg args'
  where args' = map EValue args


toString :: Value -> EvalM String
toString val = loop val 12
  where
    loop (VString str) _ = return $ string str
    loop _ 0 = throwError $ Err "RunTimeError" "Value refuses to be converted to a String" [val]
    loop val' n = do
      val'' <- eval (Call (EValue val') "to_s" [])
      loop val'' (n-1)
