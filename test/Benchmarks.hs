module Main where

import Test.Continuation
import System.CPUTime
import Text.Printf
import Control.Monad

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
  c <-  newContIO
  s <-  echo
  let d = dispatch () respNull c s "Hello World!"
  let x = send c s "Hello World!"
  time $ replicateM_ 500000 d
  time $ replicateM_ 500000 x
  time $ replicateM_ 500000 d
  time $ replicateM_ 500000 x
  putStrLn "Done!"
  
