{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import qualified Data.Map as M
import AST
import Object
import Object.Spawn
import Context
import Var
import Continuation(reply)
import Prelude hiding (lookup) 
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Exception(try, BlockedIndefinitelyOnSTM)

type Err = String

type EvalM a= StateT Context (ErrorT Err IO) a

runEvalM :: (EvalM a) -> Context -> IO (Either Err (a, Context))
runEvalM e c = do
  r <- try $ runErrorT $ runStateT e c
  case r of 
    Right x -> return x
    Left (e:: BlockedIndefinitelyOnSTM) -> return $ Left $ show e

-- |Turns an expression into a value, potentially performing
--  side effects along the way.
eval :: Exp -> EvalM Value
eval (EValue val) = return val
eval (EInt i) = return (VInt i)
eval (EFloat f) = return (VFloat f)
eval ENil = return VNil
eval ETrue = return VTrue
eval EFalse = return VFalse
eval (EIVar s) = do
  val' <- retrieveM $ simple s
  case val' of
    Just val -> return val
    Nothing  ->return VNil
eval (EAtom a) = return (VAtom a)
eval (OpStr a ops) = do
  c <- get
  eval (shunt (precedence c) [a] [] ops)
eval (EVar var) = do
  val' <- lookupM var
  case val' of
    Just val -> return val
    Nothing  -> throwError $ "Not in scope: " ++ (show var)
eval (Assign (LVar var) exp) = do
  val <- eval exp
  modify (insert var val)
  return val
eval (Assign (LIVar s) exp) = do
  val <- eval exp
  modify (insertIVar s val)
  return val
eval (Assign (LCVar s) exp) = do
  val <- eval exp
  insertCVarM s val
  return val

eval (Call expr msg args) = do
  val <- eval expr
  r <- liftIO $ valToObj val
  case r of 
    Pid pid -> do -- Remote process send a message
        vals <- mapM eval args
        dispatchM pid (Execute (simple msg) vals)
    receiver -> do
      method <- get >>= liftIO . lookupIvars (simple msg) receiver
      case method of
        Nothing -> throwError $ "Method not found: " ++ msg
        Just (VFunction fn arity) -> do -- eval args and call function
          guard $ checkArity arity $ length args 
          vals <- mapM eval args
          (result, obj') <- with receiver (fn vals)
          -- TODO: Put obj' back
          return result
        Just _ -> throwError $ "Not Implemented: cast to functions"

eval (Apply var argExprs) = do
  fn <- get >>= (liftIO  . lookup var)
  case fn of
    Nothing -> throwError $ "Not in scope: " ++ (show var)
    Just (VFunction f arity) -> if checkArity arity (length argExprs) 
      then mapM eval argExprs >>= f
      else throwError $
          "Arity Mismatch on function " ++ show var ++ 
	  " with " ++ show (length argExprs) ++" arguments."
    Just val -> eval (Call (EValue val) "call" argExprs)
eval (Def n params exp) = eval $ Assign (LCVar n) (Lambda params exp)
eval (Define lhs params exp) = eval $ Assign lhs (Lambda params exp)
eval (Lambda params exp) = return (VFunction (mkFunct params exp) (length params, Just $ length params)) -- no varargs for now
eval (Block exps) = fmap last $ mapM eval exps
eval (If pred cons (Just alt)) = do
  r <- eval pred
  if r == VNil || r == VFalse then eval alt else eval cons
eval (If pred cons Nothing) = do
  r <- eval pred
  if r == VNil || r == VFalse then return VNil else eval cons
eval (EClass n Nothing exp) = do
  -- TODO: get Object and store new class
  let cls = 
        VObject Class 
          { ivars = M.empty
	  , klass = undefined
	  , modules=[]
	  , process = Nothing
	  , super = undefined
	  , cvars = M.empty
	  , properName = name n
	  }
  Pid pid <- liftIO $ spawn cls
  sendM pid $ Eval exp
  eval $ Call (EVar Var{name="Object", scope=[]}) "setCVar" [EAtom $ name n, EValue $ VObject $ Pid pid]
  return $ VObject $ Pid pid			   
eval exp = throwError $ "Cannot yet evaluate the following expression:\n" ++ show exp

mkFunct :: [String] -> Exp -> [Value] -> EvalM Value
mkFunct params exp vals = do
  c <- get
  using (merge (zip params vals) c) (eval exp)

using :: Context -> EvalM a -> EvalM a
using c evalm = do
  cOld <- get
  put c
  resp <- evalm
  put cOld
  return resp


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

