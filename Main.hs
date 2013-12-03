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
import qualified BuiltinFunctions as F


main :: IO ()
main = do
  let 
     context =M.fromList [
        ("test", VInt 5)
      , ("add" , VFunction F.add (2,Just 2))
      , ("+"   , VFunction F.add (2,Just 2))
      , ("puts", VFunction F.puts (0,Nothing))
      ]
  repl context
 
repl :: Context -> IO ()
repl c = do
  l <- prompt
  case l of
    "" -> system c
    _  -> do
           result <- runEvalM (evaluate l) c
           case result of
             Left  err -> putStrLn ("Error: " ++ err) >> repl c
             Right (val,c') -> do
               putStrLn $ show val
               repl c'

system c = do
  l <- cmdPrompt
  case l of
    "" -> repl c
    "quit" -> return ()
    "q" -> return ()


evaluate :: String -> EvalM Value
evaluate str = case parseString str of
   Left p  -> throwError $ show p
   Right e -> eval e

prompt :: IO String
prompt = do
  putStr "Sapphire > "
  hFlush stdout
  getLines ""

cmdPrompt = do
  putStr "  System > "
  hFlush stdout
  Prelude.getLine 

getLines :: String -> IO String
getLines ls = do 
  l <- Prelude.getLine
  case l of
    "" -> return ls
    _  -> putStr "         > " >> hFlush stdout >> (getLines $ ls ++ l ++ "\n")
