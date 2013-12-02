module BuiltinFunctions where

import Control.Monad.Error
import AST
import Eval

type Fn = [Value] -> EvalM Value


add [VFloat a, VFloat b] = return $ VFloat (a + b)
add [VInt   a, VFloat b] = return $ VFloat (fromInteger a + b)
add [VFloat a, VInt   b] = return $ VFloat (a + fromInteger b)
add [VInt   a, VInt   b] = return $ VInt   (a + b)
add [_,_] = throwError "Currently only adding numbers"
add _ = throwError "Arity Error: Add takes 2 arguments"

puts vals = liftIO $ mapM_ print vals >> return VNil
