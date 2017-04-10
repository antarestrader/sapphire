{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Runtime.PID 
  ( PID
  , Message(..)
  , writePID
  , readPID
  , tID, newPID, forkPID) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map.Strict (Map)

import Name
import Runtime.Hole

data PID obj = PID {q :: TQueue (Message obj), tID :: ThreadId} deriving Eq
type ShadowMap obj = Map ThreadId (PID obj)


data Message obj = 
    Quit
  | Mark ([PID obj] -> IO())
  | Call Name [obj] (ShadowMap obj) (Hole obj) (Hole obj)

instance Show (PID a) where
  show PID{tID = x} = show x 

writePID PID{q}= writeTQueue q
readPID PID{q} = readTQueue q

newPID :: IO (PID a)
newPID = do
  q <- newTQueueIO
  PID q <$> myThreadId

forkPID :: (PID a -> IO ()) -> IO (PID a)
forkPID action = do
  q <- newTQueueIO
  tID <- forkIO $ do
    pid <- PID q <$> myThreadId
    action pid
  let pid  = PID{q,tID}
  return pid
