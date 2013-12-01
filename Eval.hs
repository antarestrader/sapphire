module Eval where

import qualified Data.Map as M
import AST
import Context
import Prelude hiding (lookup) 

eval :: Context -> Exp -> IO (Value, Context)
eval c (EInt i) = return ((VInt i), c)
eval c (EFloat f) = return ((VFloat f),c)
eval c ENil = return (VNil,c)
eval c (EAtom a) = return (VAtom a, c)
eval c (OpStr a ops) = eval c (shunt (precedence c) [a] [] ops)
eval c (EVar var) = case lookup var c of
  Just val -> return (val, c)
  Nothing  -> do -- TODO Error
    putStrLn $ "Not in scope: " ++ (show var)
    return (VNil, c)
eval c (Assign (LVar var) exp) = do
  (val, c') <- eval c exp
  return (val, insert var val c')
eval c exp = do -- TODO Error
  putStrLn "Cannot yet evaluate the following expression"
  print exp
  return (VNil,c)

-- Impliments the Shunting-yard Algorithm
-- of Edsger Dijstra as described at
-- http://www.wcipeg.com/wiki/Shunting_yard_algorithm
shunt :: (M.Map Op Precedence)  -> [Exp] -> [Op] -> [(Op,Exp)] -> Exp
shunt _ [exp] [] [] = exp
shunt c (a:b:xs) (op:ops) [] = shunt c ((Call b op [a]):xs) ops []
shunt c xs [] ((op,x):ops) = shunt c (x:xs) [op] ops
-- TODO other cases



