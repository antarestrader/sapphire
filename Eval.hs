module Eval where

import qualified Data.Map as M
import AST
import Context
import Prelude hiding (lookup) 
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

type EvalM a= StateT Context (ErrorT String IO) a

runEvalM :: (EvalM a) -> Context -> IO (Either String (a, Context))
runEvalM e c = runErrorT $ runStateT e c

eval :: Exp -> EvalM Value
eval (EInt i) = return (VInt i)
eval (EFloat f) = return (VFloat f)
eval ENil = return VNil
eval (EAtom a) = return (VAtom a)
eval (OpStr a ops) = do
  c <- get
  eval (shunt (precedence c) [a] [] ops)
eval (EVar var) = do
  val' <- gets (lookup var)
  case val' of
    Just val -> return val
    Nothing  -> throwError $ "Not in scope: " ++ (show var)
eval (Assign (LVar var) exp) = do
  val <- eval exp
  modify (insert var val)
  return val
eval exp = throwError $ "Cannot yet evaluate the following expression:\n" ++ show exp

-- Impliments the Shunting-yard Algorithm
-- of Edsger Dijstra as described at
-- http://www.wcipeg.com/wiki/Shunting_yard_algorithm
shunt :: (M.Map Op Precedence)  -> [Exp] -> [(Op,Precedence)] -> [(Op,Exp)] -> Exp
-- When the input and operator stacks are empty, the expression stack contains the result
shunt _ [exp] [] [] = exp
-- When the input stack is empty, Pop the remaining operators in order
shunt c (a:b:xs) ((op,p):ops) [] = shunt c ((makeOpExpr p b a op):xs) ops []
-- For a left associative Operator pop from the stack while operator is of strictly lower precedence
shunt c (b:a:xs) ((oPopped,pa@(p,L,_)):opstack) ((op,z):ops) | (pb < p) =
  shunt c ((makeOpExpr pa a b oPopped):xs) opstack ((op,z):ops)
  where
    (pb,_,_) = findOp c op 
-- For right- and non-associative Operator pop from the stack while operator is of lower or equal precedence
shunt c (b:a:xs) ((oPopped,pa@(p,_,_)):opstack) ((op,z):ops) | (pb <= p) =
  shunt c ((makeOpExpr pa a b oPopped):xs) opstack ((op,z):ops)
  where
    (pb,_,_) = findOp c op
-- When the operator stack is empty or the precedence is too high,
-- push the operator and next expression onto their stacks 
shunt c xs ys ((op,x):ops) = 
  shunt c (x:xs) ((op,p):ys) ops
    where
      p = findOp c op
shunt _ xs ys ops = error $ "Shunting Yard Algorithm bug:\n" ++ show xs ++ ('\n':show ys) ++ ('\n':show ops)


findOp c op = maybe (5,L,L) id (M.lookup op c)

makeOpExpr :: Precedence -> Exp -> Exp -> Op -> Exp
makeOpExpr (_,_,L) l r op = (Call l op [r])
makeOpExpr (_,_,R) l r op = (Call r op [l])
makeOpExpr (_,_,N) l r op = (Apply (Var {name=op, scope=[]}) [l,r])

