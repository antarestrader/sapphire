module Boot.ParseArgs (parseArgs) where

import System.Exit
import System.Environment

import Boot.Options
import Boot.REPL
import Object

load :: FilePath -> Runtime ()
load = undefined

parseArgs :: IO (Options, Runtime ())
parseArgs = do
  args <- getArgs
  case args of
    ["--version"] -> do
      putStrLn $ version defaultOptions
      exitSuccess
    ("-i":files) -> return (defaultOptions, (runFiles files >> repl))
    [file] -> return (defaultOptions, runFiles [file])
    _ -> return (defaultOptions, repl)

runFiles:: [String] -> Runtime ()
runFiles [] = return ()
runFiles (file : files) = do
  load file
  runFiles files
