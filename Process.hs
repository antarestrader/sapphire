{-# LANGUAGE OverloadedStrings,  NamedFieldPuns #-}
module Process where

import  Control.Monad.Except
import Data.String

import Name
import Object
import Object.Runtime
import Scope
import Eval

type Process = Name -> [Object] -> Runtime Object

evalProcess :: Runtime() -> Process -> Process
evalProcess init f name args = undefined

instanceProcess :: Name -> [Object] -> Runtime Object
instanceProcess name args= do
  r <- getMethod name args
  case r of
    Just f -> applyFn args f
    Nothing -> do
      let args' = (Prim $ VAtom name):args
      r <- getMethod "method_missing" args'
      case r of
          Just f -> applyFn args' f
          Nothing -> 
            throwError $ fromString $ "MethodMissing: "++name++" not found"

applyFn :: [Object] -> Fn -> Runtime Object
applyFn args (Fn{fn}) = o <$> (newScope $ fn args)
applyFn args (AST{params, asts}) =
      o <$> (newScope $ mkFunct params asts args) 
