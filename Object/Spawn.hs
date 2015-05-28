{-# LANGUAGE ScopedTypeVariables #-}
-- | This module impliments the consept of non local objects.  
module Object.Spawn (
    spawn
  , responderObject 
  )
where

import Prelude hiding (lookup)
import qualified Continuation as C
import Object
import Object.Graph
import {-# SOURCE #-} Eval
import AST
import Err
import Var
import Context
import qualified Data.Map as M
import Control.Monad.Error.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Control.Exception(try, BlockedIndefinitelyOnSTM)

-- | convert a object into a non-local object (aka a PID)
spawn :: Object -> IO Object
spawn obj@(Pid {}) = return obj
spawn obj | isJust (process obj) = return $ Pid $ fromJust $ process obj
spawn obj = do
  process_id <- C.respondWith obj responderObject
  C.unsafeSend process_id (Initialize process_id)
  return $ Pid process_id

-- | This is the function responsible for dealing with incomming messages
responderObject :: C.Responder Object Message Response
responderObject obj msg =
  case fst msg of
    Eval        exp -> evaluate exp obj (snd msg)
    Search IVars str -> do
      case (directIVars str obj) of
        Nothing -> C.reply (snd msg) NothingFound >> (return $ Just obj)
        Just v  -> C.reply (snd msg) (Response v) >> (return $ Just obj)
    Search CVars str -> do
      case (directCVars str obj) of
        Just v  -> C.reply (snd msg) (Response v) >> (return $ Just obj)
	Nothing -> C.reply (snd msg) NothingFound >> (return $ Just obj)
    Search Methods str -> do
      r <- run obj (snd msg) (const Nothing) (searchMethods str obj)
      case r of
        Just v  -> C.reply (snd msg) (Response v)  >> (return $ Just obj)
        Nothing ->  C.reply (snd msg) NothingFound >> (return $ Just obj)
    Search ObjectGraph str -> do
      r <-  run obj (snd msg) (const Nothing) (searchObject str (MO undefined obj)) 
      case r of
        Just (MV _ v)  -> C.reply (snd msg) (Response v) >> (return $ Just obj)
        Nothing ->  C.reply (snd msg) NothingFound >> (return $ Just obj)
    Search ClassGraph str -> do
      r <- run obj (snd msg) (const Nothing) (searchClass str obj) 
      case r of
        Just v  -> C.reply (snd msg) (Response v) >> (return $ Just obj)
        Nothing ->  C.reply (snd msg) NothingFound >> (return $ Just obj)     
    SetIVar str val -> C.reply (snd msg) (Response val) >> (return $ Just $ insertIVars str val obj)
    SetCVar str val -> C.reply (snd msg) (Response val) >> (return $ Just $ insertCVars str val obj)
    PushModule val@(VObject obj)  -> 
          C.reply (snd msg) (Response val) >> (return $ Just $ obj{modules = (obj:(modules obj))})
    PushCModule val@(VObject obj) -> case obj of
      Class{cmodules = c} -> C.reply (snd msg) (Response val) >> (return $ Just $ obj{cmodules = (obj:c)})
      _ -> C.reply (snd msg) (Error $ Err "RunTimeError" "Object is not a class" [VObject obj]) >>  (return $ Just $ obj)
    Execute     var args  -> call obj var args (snd msg)
    Initialize  pid -> do
      obj' <- initialize pid obj
      C.reply (snd msg) $ Response $ VObject $ Pid pid
      return obj'
    Terminate -> return Nothing

initialize :: Process -> Object -> IO (Maybe Object)
initialize pid obj = do
  -- TODO: initialization
  return $ Just obj{process = Just pid}

evaluate :: Exp -> Object -> Continuation -> IO (Maybe Object)
evaluate exp obj cont = do
  let context = newContext obj cont responderObject
  result <- runEvalM (eval exp) context
  case result of 
    Left  err -> C.reply cont (Response $ VError err) >> (return $ Just obj)
    Right (val,context) -> C.reply cont (Response val) >> (return $ Just (self context))  -- not proper tail call

-- WARNING: this function looses mutiations to Object if that is important
-- use evaluate instead.
run :: Object -> Continuation -> (Err Value -> a) -> EvalM a -> IO a
run obj cont fixerror action = do
  let context = newContext obj cont responderObject
  result <- runEvalM action context
  case result of 
    Left err -> return $ fixerror err
    Right (a,_) -> return a 


call :: Object -> Var -> [Value] -> Continuation -> IO (Maybe Object)
call obj var args cont = do
  let context =  newContext obj cont responderObject
  result <- runEvalM (evalT $ Apply var (map EValue args) Public) context 
  case result of
    Left  err -> (C.reply cont (Response $ VError err)) >> (return $ Just obj)
    Right ((),context) -> return $ Just (self context) 
