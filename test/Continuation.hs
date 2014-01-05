module Test.Continuation 
  ( module Test.Continuation
  , module Continuation
  ) where

import Continuation
import Control.Concurrent

squawker :: IO (ProcessId String String)
squawker = respondWith () squawker' 

squawker' :: () -> Message String String -> IO()
squawker' _ (str,cont) = do
  putStrLn str
  reply cont str
  return ()

echo = respondWith () $ \_ (m,cont) -> reply cont m >> return ()

respNull :: o -> Message m r -> IO o
respNull x _ = return x
