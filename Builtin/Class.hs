module Builtin.Class where

import Builtin.Utils
import Object
import Context
import Object.Spawn
import Context
import Eval
import AST
import Var
import String
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.IO.Class

classClass object = spawn $ VObject Class{
        ivars = M.empty,
	klass = object,
	modules = [],
	process = Nothing,
	super = object,
	cvars = bootstrap,
	properName = "Class"}

bootstrap = M.fromList [
         ("new"  , VFunction new   (0,Just 1))
       , ("setCVar", VFunction setCVar (2,Just 2))
       , ("spawn", VFunction spawnFn (0,Just 1))
       , ("to_s" , VFunction to_s (0, Just 0 ))
       ]

setCVar [VAtom n,val] = do
  slf <- gets self
  modify (\c -> c{self=slf{cvars = M.insert n val (cvars slf)}})
  replyM_ val

new [] = do
  slf <- gets self
  let obj = VObject $ Object {
      ivars = M.empty
    , klass = slf
    , modules = []
    , process = Nothing
    }
  -- initialize here
  replyM_ obj

spawnFn xs = do
  obj <- (extract $ new xs) >>= (liftIO . spawn)
  replyM_ $ VObject obj

to_s [] = do
  (VObject Class{properName = s}) <- eval (EVar Self)
  replyM_ $ VString $ mkStringLiteral $ s
