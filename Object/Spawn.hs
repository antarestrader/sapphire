{-# LANGUAGE ScopedTypeVariables #-}

module Object.Spawn where

import Prelude hiding (lookup)
import qualified Continuation as C
import Object
import Object.Graph
import {-# SOURCE #-} Eval
import AST
import Var
import Context
import qualified Data.Map as M
import Control.Monad.Error.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Control.Exception(try, BlockedIndefinitelyOnSTM)

spawn :: Value -> IO Object
spawn (VObject obj@(Pid {})) = return obj
spawn (VObject obj) | isJust (process obj) = return $ Pid $ fromJust $ process obj
spawn (VObject obj) = do
  process_id <- C.respondWith obj responderObject
  C.unsafeSend process_id (Initialize process_id)
  return $ Pid process_id
spawn val = do
  process_id <- C.respondWith val responderPrim
  return $ Pid process_id

responderPrim :: C.Responder Value Message Value
responderPrim val msg = do  --TODO actually do something here
  C.reply (snd msg) $ val
  return val

responderObject :: C.Responder Object Message Value
responderObject obj msg =
  case fst msg of
    Eval        exp -> evaluate exp obj (snd msg)
    SearchIVars str -> do
      case (lookupIVarsLocal str obj) of
        Nothing -> C.reply (snd msg) (VError "not found") >> return obj
        Just v  -> C.reply (snd msg) v >> return obj
    SearchCVars str -> undefined -- TODO
    SetIVar str val -> C.reply (snd msg) val >> (return $ insertIVarLocal str val obj)
    SetCVar str val -> C.reply (snd msg) val >> (return $ insertCVarLocal str val obj)
    Execute     var args  -> call obj var args (snd msg)
    Initialize  pid -> do
      obj' <- initialize pid obj
      C.reply (snd msg) $ VObject $ Pid pid
      return obj'

initialize :: Process -> Object -> IO Object
initialize pid obj = do
  -- TODO: initialization
  return obj{process = Just pid}

evaluate :: Exp -> Object -> Continuation -> IO Object
evaluate exp obj cont = do
  let context = newContext obj cont responderObject
  result <- runEvalM (eval exp) context
  case result of 
    Left  err -> C.reply cont (VError err) >> return obj
    Right (val,context) -> C.reply cont val >> return (self context)  -- not proper tail call

call :: Object -> Var -> [Value] -> Continuation -> IO Object
call obj var args cont = do
  let context =  newContext obj cont responderObject
  result <- flip runEvalM context $ do
    method <- lookupVar var
    case method of 
      (VFunction fn _) -> fn args
      (VError _ ) ->  throwError $  "Method missing" ++ show var
      _ -> throwError "Method cast not yet implimented"
  case result of
        Left  err -> (C.reply cont (VError err)) >> return obj
        Right (val,context) -> (C.reply cont val) >> return (self context) 



