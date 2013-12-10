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
import Boot(boot)

main :: IO ()
main = do
  main <- boot
  let context = Context {locals = M.empty, self=main, continuation = undefined}
  repl context
 
repl :: Context -> IO ()
repl c = do
  l <- prompt "Sapphire"
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
  l <- prompt "parser"
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
  l <- prompt "tokens"
  case l of
    "" -> system c
    _  -> do
           let toks = scanBlock $ parseCode "Input String" l
           print toks 
           tokenREPL c


system c = do
  l <- cmdPrompt "System"
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

prompt :: String -> IO String
prompt l = do
  let l' = (replicate (9 - length l) ' ') ++ l
  putStr $ l' ++" > "
  hFlush stdout
  getLines ""

cmdPrompt l = do
  let l' = (replicate (9 - length l) ' ') ++ l
  putStr $ l' ++" > "
  hFlush stdout
  Prelude.getLine

getLines :: String -> IO String
getLines ls = do 
  l <- Prelude.getLine
  case l of
    "" -> return ls
    _  -> putStr "          > " >> hFlush stdout >> (getLines $ ls ++ l ++ "\n")
