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
        ivars = M.fromList [("setClass",VFunction F.setClass (1,Just 1))],
        klass = ROOT,
        modules = [],
	process = Nothing,
        super = ROOT,
        cvars = bootstrapset,
        properName = "Object"}
  cls <- spawn $ VObject Class{
        ivars = M.empty,
	klass = object,
	modules = [],
	process = Nothing,
	super = object,
	cvars = M.empty,
	properName = "Class"}
  cont <- newEmptyMVar
  writeChan (channel object) (Execute Var{name="setClass", scope=[]} [VObject cls] cont)
  -- FIXME: circular call structure is causing deadlock
  -- writeChan (channel object) (Execute Var{name="setCVar", scope=[]}  [VAtom "Object", VObject object] cont)
  -- writeChan (channel object) (Execute Var{name="setCVar", scope=[]}  [VAtom "Class", VObject cls] cont)
  return $ Object {ivars = M.empty, modules=[], klass = object, process=Nothing}

bootstrapset = M.fromList [
         ("test", VInt 5)
       , ("add" , VFunction F.add  (2,Just 2))
       , ("+"   , VFunction F.add  (2,Just 2))
       , ("-"   , VFunction F.sub  (2,Just 2))
       , ("*"   , VFunction F.mult (2,Just 2))
       , ("puts", VFunction F.puts (0,Nothing))
       , ("cls" , VFunction F.cls  (0, Just 0))
       , ("setCVar" , VFunction F.setCVar  (2, Just 2))
       ]
