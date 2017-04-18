module Boot.ParseArgs (parseArgs) where

import System.Exit
import System.Environment

import Boot.Options
import Boot.REPL
import Object
import Object.Runtime
import Scope

load :: FilePath -> Runtime Response
load = undefined

parseArgs :: IO (Options, Runtime Response)
parseArgs = do
  args <- getArgs
  case args of
    ["--version"] -> do
      putStrLn $ version defaultOptions
      exitSuccess
    ("-i":files) -> return (defaultOptions, (runFiles files >> repl))
    [file] -> return (defaultOptions, runFiles [file])
    _ -> return (defaultOptions, repl)

runFiles:: [String] -> Runtime Response
runFiles [] = reply Nil
runFiles (file : files) = do
  load file
  runFiles files
