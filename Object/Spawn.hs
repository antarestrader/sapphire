module Object.Spawn where

import Object
import Object.Graph
import {-# SOURCE #-} Eval
import AST
import Var
import Context
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Maybe

spawn :: Value -> IO Object
spawn (VObject obj@(Pid {})) = return obj
spawn (VObject obj) | isJust (process obj) = return $ fromJust $ process obj
spawn (VObject obj) = do
  chan <- newChan
  tid  <- forkIO (initialize chan obj)
  return $ Pid chan tid
spawn v = do
  chan <- newChan
  tid  <- forkIO (respondPrim chan v)
  return $ Pid chan tid

initialize chan obj = do
  tid <- myThreadId
  respond chan obj{process = Just (Pid chan tid)}

respond :: Chan Message -> Object -> IO ()
respond chan obj = do
  msg <- readChan chan
  case msg of
    Terminate -> return ()
    Eval exp  -> evaluate exp obj >>= respond chan
    Search      var cont -> search var obj cont >> respond chan obj
    SearchClass var cont -> search' var obj cont >> respond chan obj
    Retrieve    var cont -> retrieve var obj cont >> respond chan obj
    Execute     var args cont -> call obj var args cont >>= respond chan

respondPrim chan obj = do
  msg <- readChan chan
  case msg of
    Terminate -> return ()
    _ -> respondPrim chan obj -- TODO: actually respond

evaluate :: Exp -> Object -> IO Object
evaluate exp obj = do
  c <- newEmptyMVar
  let context = Context {locals = M.empty, self = obj, continuation = c}
  result <- runEvalM (eval exp) context
  case result of 
    Left  err -> fail err
    Right (_,Context {self = obj'}) -> return obj'

call :: Object -> Var -> [Value] -> (MVar Value) -> IO Object
call obj var args cont = do
  method <- getMethod obj var
  let context = Context {locals = M.empty, self = obj, continuation = cont}
  result <- runEvalM (method args) context
  case result of
    Left  err -> tryPutMVar cont (VError err) >> return obj
    Right (val,Context{self=obj'}) -> tryPutMVar cont val >> return obj' 

getMethod obj var = do
  val <- cps $ search var obj
  case val of
    Just (VFunction fn _) -> return fn
    Just _ -> fail "Function casting not yet implimented"
    Nothing -> fail "method not in scope"

