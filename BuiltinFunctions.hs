-- Copyright 2013, 2014 John F. Miller
{-# Language RankNTypes #-}

-- | All Functions that are primitive to the sapphire and must be provided by
--   the run time system.
module BuiltinFunctions where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import Object
import qualified Object.Spawn as S
import Eval
import Context

binop :: (forall a. Num a => a->a->a) -> [Value] -> EvalM ()
binop op [VFloat a, VFloat b] = replyM_ $ VFloat (a `op` b)
binop op [VInt   a, VFloat b] = replyM_ $ VFloat (fromInteger a `op` b)
binop op [VFloat a, VInt   b] = replyM_ $ VFloat (a `op` fromInteger b)
binop op [VInt   a, VInt   b] = replyM_ $ VInt   (a `op` b)
binop op [_,_] = throwError "Attempting to add non numeric data"
binop op _ = throwError "Arity Error: Add takes 2 arguments"

add  = binop (+)
sub  = binop (-)
mult = binop (*)

puts :: [Value] -> EvalM ()
puts vals = (liftIO $ mapM_ print vals) >> replyM_ VNil

cls [] = gets (klass . self) >>= (replyM_ . VObject)

setClass [VObject cls] = do
  slf <- gets self
  modify (\c -> c{self=slf{klass=cls}})
  replyM_ VNil

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

spawn xs = do
  obj <- (extract $ new xs) >>= (liftIO . S.spawn)
  replyM_ $ VObject obj
