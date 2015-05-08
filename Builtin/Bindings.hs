module Builtin.Bindings
  (bindPrimitiveObject, initialize) 
where

import {-# SOURCE #-} Eval
import AST
import Var
import Context
import Builtin.Array
import Builtin.Hash
import Builtin.Integer
import Builtin.Real
import Builtin.Bool
import Object
import qualified Data.Map as M
import Control.Monad.Except

initialize :: [Value] -> EvalM ()
initialize _ = do
  arrayClass
  hashClass
  integerClass
  realClass
  boolClasses
  replyM_ VNil

bindPrimitiveObject ::  Value -> EvalM Object
bindPrimitiveObject val@(VInt _) = do
  cls <- getPrimClass "Integer" 
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VFloat _) = do
  cls <- getPrimClass "Real" 
  return $ buildPrimInstance cls val
bindPrimitiveObject VNil = do
  cls <- getPrimClass "NilClass"
  return $ buildPrimInstance cls VNil
bindPrimitiveObject val@(VString _) = do
  cls <- getPrimClass "String"
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VArray _) = do
  cls <- getPrimClass "Array"
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VFunction{}) = do
  cls <- getPrimClass "Function"
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VHash _) = do
  cls <- getPrimClass "Hash"
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VTrue) = do
  cls <- getPrimClass "TrueClass"
  return $ buildPrimInstance cls val
bindPrimitiveObject val@(VFalse) = do
  cls <- getPrimClass "FalseClass"
  return $ buildPrimInstance cls val
bindPrimitiveObject val = throwError $ "No Class for this type: " ++ show val --TODO impliment classes

-- | lookup the class for primitive values in the current context.
--   (internal function)
getPrimClass :: String -> EvalM Object
getPrimClass str = do
  cls' <- (eval $ EVar $ simple str)
  case cls' of
    (VObject obj) -> return obj
    _ -> throwError $ "System Error: Primitive class not found: " ++ str

-- | Given the primitive class and a primitive value build in instance.
--   (internal function)
buildPrimInstance :: Object -- the class
                  -> Value  -- the value
                  -> Object
buildPrimInstance cls val = Object 
                          { ivars = M.singleton "__value" val
                          , klass = cls
                          , modules = []
                          , process = Nothing
                          }

