module Main where

import System.Environment
import System.IO
import Control.Monad.Error
import qualified Data.Map as M
import LineParser
import AST
import Parser
import Eval
import Context


main :: IO ()
main = do
  let context = M.fromList [("test",VInt 5)]
  repl context
 
repl :: Context -> IO ()
repl c = do
  l <- prompt
  result <- runEvalM (evaluate l) c
  case result of
    Left  err -> putStrLn ("Error: " ++ err) >> repl c
    Right (val,c') -> do
      putStrLn $ show val
      repl c'

evaluate :: String -> EvalM Value
evaluate str = case parseString str of
   Left p  -> throwError $ show p
   Right e -> eval e

prompt :: IO String
prompt = do
  putStr "Sapphire > "
  hFlush stdout
  Prelude.getLine
