{-# LANGUAGE CPP #-}
module Main where

import System.Environment
import System.Exit
import System.IO
import Control.Monad.Except
import qualified Data.Map as M

import Boot.Options
import Boot.ParseArgs
import Boot
import Object


main :: IO ()
main = do
  (opts,run) <- parseArgs
  putStrLn $ version opts
  boot opts run

