{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Control.Monad.Except
import Object
import Var
import AST
import Data.Map as M

class Error e where
  strMsg :: String -> e
  noMsg :: e
  noMsg = strMsg ""

instance Error String where
  strMsg = id 

guardR :: (Error e, MonadError e m)=> String -> Bool -> m ()
guardR _ True = return ()
guardR msg False = throwError $ strMsg msg

-- | Impliments the Shunting-yard Algorithm
--   of Edsger Dijstra as described at
--   http://www.wcipeg.com/wiki/Shunting_yard_algorithm
shunt :: (M.Map Op Precedence) -- the operator presedence table
      -> [Exp]                 -- the expression stack (preload with first expression)
      -> [(Op,Precedence)]     -- the operator stack (initially empty)
      -> [(Op,Exp)]            -- the input stack (initially the all op exp pairs)
      -> Exp                   -- the resulting expression tree
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
