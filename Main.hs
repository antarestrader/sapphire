module Main where

import System.Environment
import System.IO
import Control.Monad.Error
import qualified Data.Map as M
import LineParser (parseCode)
import Tokens
import Object
import Object.Spawn (responderObject)
import AST
import Parser
import Eval
import Context
import Continuation(newContIO)
import Var (simple)
import Boot(boot)
import Text.Regex.Posix

main :: IO ()
main = do
  main <- boot
  context <- newContextIO main responderObject
  interperter ["base/base.sap"] context
  -- interperter [""] context

repl :: Context -> IO ()
repl c = do
  l <- prompt "Sapphire"
  case l of
    "" -> system c
    _  -> do
           result <- flip runEvalM c $ do
                     r <- evaluate l
                     eval $ Apply (simple "puts") [EValue r]
                     return r
           case result of
             Left  err -> putStrLn ("Error: " ++ err) >> repl c
             Right (val,c') -> do
               -- putStrLn $ show val
               repl $ merge [("it", val)] c'

parserREPL :: Context -> IO ()
parserREPL c = do
  l <- prompt "parser"
  case l of
    "" -> system c
    _  -> do
           let result = parseString l
           case result of
             Left  err -> putStrLn ("Error: " ++ err) >> parserREPL c
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

data Command = Cmd{regex::String, help_text::String, fn :: ([String] -> Context -> IO())}

commands :: [Command]
commands = [
    Cmd "sapphire[ ]*(.*)|^$" "sapphire [file]:  run the inturperter with file"                      interperter
  , Cmd "parser[ ]*(.*)"      "parser [file]  :  parse input (or file) and display the AST"          parser
  , Cmd "scan[ ]*(.*)"        "scan [file]    :  lex the input or file and display the token stream" scanner
  , Cmd "(q|quit)"            "[q]uit         :  exit the program"     quiter
  , Cmd "(h|help|\\?)"        "[h]elp (?)     :  display this message" helper
  , Cmd "load (.*)"           "load <file>    :  load and evaluate the file" interperter
  , Cmd "debug[ ]*(.*)"       "debug [Object] :  print IVars for Object or local binding is ommited" debugger
  ]

interperter [""] c = repl c
interperter [file] c = do
  r <- parseFile file
  case r of
    Left err -> putStrLn (show err)    >> system c
    Right exps -> do
      r' <- runEvalM (eval $ Block exps) c
      case r' of
        Left err -> putStrLn err >> system c
        Right (res,c') -> putStrLn (show res) >> repl c'

parser [""] c = parserREPL c
parser [file] c = do
  l <- parseFile file
  case l of
    Left err -> putStrLn (show err)    >> system c
    Right exps -> putStrLn (show exps) >> system c

scanner [""] c = tokenREPL c
scanner [file] c = do
   l <- readFile file
   let toks = scanBlock $ parseCode file l
   print toks
   system c

quiter _ _ = return ()

helper _ c = do
  mapM_ (\x -> putStrLn (help_text x)) commands
  system c


system c = do
  l <- cmdPrompt "System"
  let m :: String -> [Command] -> IO ()
      m _ [] = putStrLn "Command not found" >> helper [] c
      m l (x:xs) = case (l =~ regex x) of
        [] -> m l xs
        [ys] -> (fn x) (tail ys) c
  m l commands

debugger [""] c = do
  flip runEvalM c $ do
    m <- debugM
    liftIO $ putStrLn $ show m
  interperter [""] c
debugger [obj] c = do
  flip runEvalM c $ do
    val <- eval (EVar $ simple obj)
    case val of
      VObject (Object{ivars = i}) -> do
        liftIO $ putStrLn $ show i
      VObject (Class{ivars = i, cvars = c}) -> liftIO $ do
        putStrLn "IVars:"
        putStrLn $ show i
        putStrLn "CVars:"
        putStrLn $ show c
      v -> liftIO $ putStrLn $ show v
  interperter [""] c

evaluate :: String -> EvalM Value
evaluate str = case parseString str of
   Left p  -> throwError $ p
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
