module Boot where

import Var
import Object
import Object.Spawn
import Context
import Control.Monad.State
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified BuiltinFunctions as F


boot :: IO Object
boot = do
  object <- spawn $ VObject Class{
        ivars = M.fromList [("setClass",VFunction setClass (1,Just 1))],
        klass = ROOT,
        modules = [],
        super = ROOT,
        cvars = bootstrapset,
        properName = "Object"}
  cls <- spawn $ VObject Class{
        ivars = M.empty,
	klass = object,
	modules = [],
	super = object,
	cvars = M.empty,
	properName = "Class"}
  cont <- newEmptyMVar
  writeChan (channel object) (Execute Var{name="setClass", scope=[]} [VObject cls] cont)
  return $ Object {ivars = M.empty, modules=[], klass = object}

setClass [VObject cls] = do
  slf <- gets self
  modify (\c -> c{self=slf{klass=cls}})
  return VNil

bootstrapset = M.fromList [
         ("test", VInt 5)
       , ("add" , VFunction F.add  (2,Just 2))
       , ("+"   , VFunction F.add  (2,Just 2))
       , ("-"   , VFunction F.sub  (2,Just 2))
       , ("*"   , VFunction F.mult (2,Just 2))
       , ("puts", VFunction F.puts (0,Nothing))
       ]
