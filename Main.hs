module Main where

import System.Environment
import LineParser

main :: IO ()
main = do
  args <- getArgs
  l <- lineFile $ head args
  print $ length l

