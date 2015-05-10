module Builtin.Directory where

import Builtin.Utils
import String
import Err
import Eval
import Object
import Var
import Context (replyM_)
import qualified Data.Map as M
import qualified Array as A

import Control.Monad.IO.Class
import System.Directory

directoryClass :: EvalM Object
directoryClass = buildClassEx "Directory" M.empty bootstrap

bootstrap = M.fromList [
         ("canonical", VFunction canonical (1, Nothing))
       -- , ("ls", VFunction list (1,Just 1))
       ]

canonical :: [Value] -> EvalM ()
canonical [(VString s)] = do
  canonicalDirectory <- liftIO $ canonicalizePath $ string s
  replyM_ $ VString $ mkStringLiteral canonicalDirectory
canonical [dir] = do
  (VString s) <- call dir "to_s" []
  canonicalDirectory <- liftIO $ canonicalizePath $ string s
  replyM_ $ VString $ mkStringLiteral canonicalDirectory 
canonical dirs = do
  dirs' <- mapM (\v -> call v "to_s" []) dirs
  let strs = map (\(VString s) -> string s) dirs'
  canonicalDirs <- liftIO $ mapM canonicalizePath strs
  let obj = VArray $ A.fromList $ map (VString . mkStringLiteral) canonicalDirs
  replyM_ obj


