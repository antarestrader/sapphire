module Main where

import System.Environment
import System.IO
import Control.Monad.Error
import qualified Data.Map as M
import LineParser
import Tokens
import Object
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
      , ("add" , VFunction F.add  (2,Just 2))
      , ("+"   , VFunction F.add  (2,Just 2))
      , ("-"   , VFunction F.sub  (2,Just 2))
      , ("*"   , VFunction F.mult (2,Just 2))
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
               repl $ merge [("it", val)] c'

parserREPL :: Context -> IO ()
parserREPL c = do
  l <- prompt
  case l of
    "" -> system c
    _  -> do
           let result = parseString l
           case result of
             Left  err -> putStrLn ("Error: " ++ show err) >> parserREPL c
             Right exps -> do
               mapM_ print exps 
               parserREPL c

tokenREPL :: Context -> IO ()
tokenREPL c = do
  l <- prompt
  case l of
    "" -> system c
    _  -> do
           let toks = scanBlock $ parseCode "Input String" l
           print toks 
           tokenREPL c


system c = do
  l <- cmdPrompt
  case l of
    "" -> repl c
    "parser" -> parserREPL c
    "tokens" -> tokenREPL  c
    "quit" -> return ()
    "q" -> return ()
    _ -> putStrLn "Unknown Command." >> system c

evaluate :: String -> EvalM Value
evaluate str = case parseString str of
   Left p  -> throwError $ show p
   Right es -> fmap last $ mapM eval es

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
