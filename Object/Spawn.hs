{-# LANGUAGE ScopedTypeVariables #-}

module Object.Spawn where

import Object
import Object.Graph
import {-# SOURCE #-} Eval
import AST
import Var
import Context
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Control.Exception(try, BlockedIndefinitelyOnSTM)

spawn :: Value -> IO Object
spawn (VObject obj@(Pid {})) = return obj
spawn (VObject obj) | isJust (process obj) = return $ fromJust $ process obj
spawn (VObject obj) = do
  chan <- newTChanIO
  tid  <- forkIO (initialize chan obj)
  return $ Pid chan tid
spawn v = do
  chan <- newTChanIO
  tid  <- forkIO (respondPrim chan v)
  return $ Pid chan tid

initialize chan obj = do
  tid <- myThreadId
  err <- try $ respond chan obj{process = Just (Pid chan tid)}
  case err of
    Right _ -> return ()
    Left (e:: BlockedIndefinitelyOnSTM) -> putStrLn $ "STM Blocked " ++ show tid

respond :: TChan Message -> Object -> IO ()
respond chan obj = do
  msg' <- try $ atomically $ readTChan chan
  msg <- case msg' of
    Right m -> return m
    Left (e:: BlockedIndefinitelyOnSTM) -> do
      tid <- myThreadId
      putStrLn $ "STM Blocked reading message " ++ show tid
      fail "dieing"
  case msg of
    Terminate -> return ()
    Eval exp  -> evaluate exp obj >>= respond chan
    Search      var cont -> (atomically $ search var obj cont)   >> respond chan obj
    SearchClass var cont -> (atomically $ search' var obj cont)  >> respond chan obj
    Retrieve    var cont -> (atomically $ retrieve var obj cont) >> respond chan obj
    Execute     var args cont -> call obj var args cont >>= respond chan

respondPrim chan obj = do
  msg <- atomically $ readTChan chan
  case msg of
    Terminate -> return ()
    _ -> respondPrim chan obj -- TODO: actually respond

evaluate :: Exp -> Object -> IO Object
evaluate exp obj = do
  c <- newEmptyTMVarIO
  let context = Context {locals = M.empty, self = obj, continuation = c}
  result <- runEvalM (eval exp) context
  case result of 
    Left  err -> fail err
    Right (_,Context {self = obj'}) -> return obj'

call :: Object -> Var -> [Value] -> (TMVar Value) -> IO Object
call obj var args cont = do
  method <- do 
    r <- try $ getMethod obj var
    case r of
      Right m -> return m
      Left (e:: BlockedIndefinitelyOnSTM) -> fail $ "STM Blocked getting method" ++ show var
  let context = Context {locals = M.empty, self = obj, continuation = cont}
  result <- runEvalM (method args) context
  case result of
    Left  err -> (atomically $ tryPutTMVar cont (VError err)) >> return obj
    Right (val,Context{self=obj'}) -> (atomically $ tryPutTMVar cont val) >> return obj' 

getMethod obj var = do
  val <- cps $ search var obj -- this causes dead-lock if search loops around
  case val of
    Just (VFunction fn _) -> return fn
    Just _ -> fail "Function casting not yet implimented"
    Nothing -> fail "method not in scope"

