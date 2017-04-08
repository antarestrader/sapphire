-- Object/Runtime.hs Copyright 2017 John F. Miller
{-# OPTIONS_GHC -fno-warn-missing-methods#-}
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, TypeSynonymInstances,
    MultiParamTypeClasses, RankNTypes, LiberalTypeSynonyms,
    FlexibleInstances #-}
module Object.Runtime where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map.Strict
import Data.String

import Object
import Scope
import qualified Runtime as R hiding (Runtime)
import Name
import Eval

type Process = Name -> [Object] -> Runtime Object

instance Scope Runtime where
  readVar IVar name = do
    map <- ivars . objectState <$> R.getState
    return
      $   (\obj -> MV {obj,replace=(setVar IVar name)})
      <$> lookup name map
  readVar Local name = do
    scp <- localScope <$> R.getState
    let loop :: [Namespace Object] -> Int -> Maybe (Value Runtime)
        loop [] _ = Nothing
        loop (x:xs) i = case lookup name x of
          Nothing -> loop xs (i+1)
          Just obj -> Just MV{obj,replace=rep i}
        rep :: Int -> Object -> Runtime ()
        rep i obj = do
            ss <- R.getState
            let scp'= loop' i (localScope ss)
                loop' :: Int -> [Namespace Object] -> [Namespace Object]
                loop' _ [] = []
                loop' 0 (x:xs) = ((insert name obj x):xs)
                loop' i (x:xs) = (x:(loop' (i-1) xs))
            R.putState ss{localScope=scp'}
    return $ loop scp 0

  spawn (Process pid) = return $ Process pid
  spawn (Object st) = do
    uids <- uidSource <$> R.getState
    pid <- R.spawn
        SystemState{
            objectState = st
          , uidSource = uids
          , localScope = []
        }
        (processForState st)
    send pid "initialize" [] (\_->return ())
    return $ Process pid
  spawn prim = undefined

evalProcess :: Runtime() -> Process -> Process
evalProcess init f "initialize" args = init >> f "initialize" args
evalProcess _ f name args = f name args
-- The problem here is that we can never forget init
-- I still would like to be able to mutate an object's Process

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

-- =============PRIVATE======================
-- ------------------------------------------

applyFn :: [Object] -> Fn -> Runtime Object
applyFn args (Fn{fn}) = o <$> (newScope $ fn args)
applyFn args (AST{params, asts}) =
      o <$> (newScope $ mkFunct params asts args)

processForState :: State -> Process
processForState Instance{} = instanceProcess
