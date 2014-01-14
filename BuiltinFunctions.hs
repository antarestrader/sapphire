{-# Language RankNTypes #-}
module BuiltinFunctions where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import Object
import qualified Object.Spawn as S
import Eval
import Context

binop :: (forall a. Num a => a->a->a) -> [Value] -> EvalM Value
binop op [VFloat a, VFloat b] = return $ VFloat (a `op` b)
binop op [VInt   a, VFloat b] = return $ VFloat (fromInteger a `op` b)
binop op [VFloat a, VInt   b] = return $ VFloat (a `op` fromInteger b)
binop op [VInt   a, VInt   b] = return $ VInt   (a `op` b)
binop op [_,_] = throwError "Currently only adding numbers"
binop op _ = throwError "Arity Error: Add takes 2 arguments"

add  = binop (+)
sub  = binop (-)
mult = binop (*)

puts vals = liftIO $ mapM_ print vals >> return VNil

cls [] = gets (klass . self) >>= (return . VObject)

setClass [VObject cls] = do
  slf <- gets self
  modify (\c -> c{self=slf{klass=cls}})
  return VNil

setCVar [VAtom n,val] = do
  slf <- gets self
  modify (\c -> c{self=slf{cvars = M.insert n val (cvars slf)}})
  return val

new [] = do
  slf <- gets self
  let obj = VObject $ Object {
      ivars = M.empty
    , klass = slf
    , modules = []
    , process = Nothing
    }
  -- initialize here
  return obj

spawn xs = do
  obj <- new xs >>= liftIO . S.spawn
  return $ VObject obj
