module Main where

import System.Environment
import System.IO
import LineParser
import Parser
import Eval


main :: IO ()
main = do
  let context = emptyContext
  repl context
 
repl :: Context -> IO ()
repl c = do
  l <- prompt
  (val,c') <- evaluate c l
  putStrLn $ show val
  repl c'

evaluate :: Context -> String -> IO (Value, Context)
evaluate c str = case parseString str of
  Left p  -> print p >> return (VNil, c)
  Right e -> eval c e

prompt :: IO String
prompt = do
  putStr "Sapphire > "
  hFlush stdout
  Prelude.getLine
