module Object.Spawn where

import Object
import {-# SOURCE #-} Eval
import AST
import Var
import Context
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

spawn :: Value -> IO Pid
spawn (VPid p) = return p
spawn (VObject obj) = do
  chan <- newChan
  tid  <- forkIO (respond chan obj)
  return $ Pid chan tid
spawn v = do
  chan <- newChan
  tid  <- forkIO (respondPrim chan v)
  return $ Pid chan tid

respond :: Chan Message -> Object -> IO ()
respond chan obj = do
  msg <- readChan chan
  case msg of
    Terminate -> return ()
    Eval exp  -> evaluate exp obj >>= respond chan
    Retrieve var cont -> getValue' obj var cont >> respond chan obj 
    Execute var args cont -> call obj var args cont >>= respond chan

respondPrim chan obj = do
  msg <- readChan chan
  case msg of
    Terminate -> return ()
    _ -> respondPrim chan obj -- TODO: actually respond

evaluate :: Exp -> Object -> IO Object
evaluate exp obj = do
  c <- newEmptyMVar
  let context = Context {locals = M.empty, self = Right obj, continuation = c}
  result <- runEvalM (eval exp) context
  case result of 
    Left  err -> fail err
    Right (_,Context {self = Right obj'}) -> return obj'

call :: Object -> Var -> [Value] -> (MVar Value) -> IO Object
call obj var args cont = do
  method <- getMethod obj var
  let context = Context {locals = M.empty, self = Right obj, continuation = cont}
  result <- runEvalM (method args) context
  case result of
    Left  err -> tryPutMVar cont (VError err) >> return obj
    Right (val,Context{self=Right obj'}) -> tryPutMVar cont val >> return obj' 

getMethod = undefined
getValue' = undefined
