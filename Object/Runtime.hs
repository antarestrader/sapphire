-- Object/Runtime.hs Copyright 2017 John F. Miller
{-# OPTIONS_GHC -fno-warn-missing-methods#-}
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, TypeSynonymInstances,
    MultiParamTypeClasses, RankNTypes, LiberalTypeSynonyms,
    FlexibleInstances #-}
module Object.Runtime where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad.State hiding (State, state, ap)
import Control.Monad.Except hiding (ap)
import Control.Monad.STM (atomically)
import Data.Map.Strict as M
import Data.String

import Object
import Scope hiding (Class)
import qualified Runtime as R hiding (Runtime)
import qualified Runtime.Runtime as RR
import Name
import Eval
import qualified Object.UID as UID

-- Process (this filled our version of RR.Fn) is the function that runs at
-- the heart of each process.
type Process = Name -> [Object] -> Runtime RR.Response
(<||>) = liftA2 (<|>)
infixl 3 <||>

instance (MonadState State) Runtime where
  get = objectState <$> R.getState
  put st = R.getState >>= (\ss -> R.putState ss{objectState = st})

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

  getMethod name args = local <||> remote
    where
      local :: Runtime (Maybe Fn)
      local = gets (M.lookup name . localCache)
      remote :: Runtime (Maybe Fn)
      remote = do
        cls <- gets instanceOfClass
        res <- call (Just $ Pointer cls) "getMethod" (Prim (VAtom name):args)
        return $ case o res of
          VFunction{function=fn} -> Just fn
          otherwise -> Nothing

  spawn (Process pid) = return pid
  spawn (Object st) = do
    ss <- R.getState
    pid <- R.spawn
        ss {
            objectState = st
          , localScope = []
          }
        (processForState st)
    send pid "initialize" [] (\_->return ())
    return  pid
  spawn prim = undefined

  newScope act = do
    ss <- R.getState
    R.putState ss{localScope = (M.empty : (localScope ss))}
    obj <- R.apply Nil act
    ss' <- R.getState
    R.putState ss'{localScope = tail (localScope ss)}
    return $ v obj

  nextUID = do
    ss <- R.getState
    RR.Runtime $ liftIO $ atomically $ UID.nextUID $ uidSource ss

-- | a process that will first evaluate an action
evalProcess :: Runtime() -- ^ Run this first
            -> Process  -- ^ Then act like this
            -> Process  -- ^ resultes in this overall process
evalProcess init f name args = reset >> init >> f name args
  where
    reset :: Runtime ()
    reset = RR.Runtime $ modify (\rts -> rts{RR.fn=f})

-- | the specialized process used to actually run the source code of the
--   program.  It is run in the context of an instance of Object.
mainProcess :: Runtime Response -> Process
mainProcess prgm "run" _ = prgm
mainProcess _ name args = instanceProcess name args

-- | the basic process of most objects.
instanceProcess :: Name -> [Object] -> Runtime Response
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

-- | A specialized process that handles (or will) some special properties
--   of classes while defering everything else to `instanceProcess`.
classProcess :: Process
classProcess "getMethod" args@(Prim (VAtom name):_) = do
    r <- method <||> local
    case r of
      Just fn -> reply $ VFunction {function = fn, cacheable = False, fUID=0}
      Nothing -> remote
  where
    method = gets (lookup name . methods)
    local = gets (lookup name . methodCache)
    remote = do
      cls <- gets superClass
      tailCall (Just $ Pointer cls) "getMethod" args
classProcess name args = instanceProcess name args

-- =============PRIVATE======================
-- ------------------------------------------

-- | unwraps the Fn datatype, applies the arguments, and evaluates
--   the function.
applyFn :: [Object] -> Fn -> Runtime Response
applyFn args (Fn{fn}) = fn args
applyFn args (AST{params, asts}) = mkFunct params asts args

-- | Selectes the appropriate Process for a state
processForState :: State -> Process
processForState Instance{} = instanceProcess
processForState Class{} = classProcess


