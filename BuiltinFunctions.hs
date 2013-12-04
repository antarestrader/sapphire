{-# Language RankNTypes #-}
module BuiltinFunctions where

import Control.Monad.Error
import Object

type Fn = [Value] -> EvalM Value


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
