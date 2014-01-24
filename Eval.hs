-- Copyright 2013, 2014 John F. Miller
{-# LANGUAGE ScopedTypeVariables #-}


module Eval (
    EvalM
  , Err
  , runEvalM
  , eval
  , evalT
  )
where

import qualified Data.Map as M
import Data.Maybe
import AST
import Object
import Object.Graph
import Object.Spawn
import Context
import Var
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
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
eval (EAtom a) = return (VAtom a)

eval (EIVar s) = do
  val <- gets self >>= lookupIVarsM s 
  return $ maybe VNil id val
eval (EVar var) = lookupVar var

eval (OpStr a ops) = do
  c <- get
  eval (shunt (precedence c) [a] [] ops)

eval (Assign lhs exp) = do
  val <- eval exp
  insertLHS lhs val
  return val

eval (Call expr msg args) = do
  val <- eval expr
  r <- liftIO $ valToObj val
  vals <- mapM eval args
  case r of 
    Pid pid -> dispatchM pid (Execute (simple msg) vals)
    receiver -> fmap fst $ with receiver $ do 
      (method, arity) <- fnFromVar (simple msg)
      guard $ checkArity arity $ length vals
      extract $ method vals -- proper tail calls here
    -- TODO put self back (see issue #28 on github)

eval (Apply var args) = do
  (fn, arity) <- fnFromVar var
  guard $ checkArity arity $ length args
  vals <- mapM eval args
  extract $ fn vals -- extract impliments proper tail recursion

eval (Def n params exp) = eval $ Assign (LCVar n) (Lambda params exp)
eval (Define lhs params exp) = eval $ Assign lhs (Lambda params exp)
eval (Lambda params exp) = return (VFunction (mkFunct params exp) (length params, Just $ length params)) -- no varargs for now

eval (Block exps) = fmap last $ mapM eval exps

eval (If pred cons alt) = do
  r <- eval pred
  if r == VNil || r == VFalse then maybe (return VNil) eval alt else eval cons

eval (EClass n super exp) = do
  cls <- eval (EVar n)
  case cls of 
    VError _ -> buildClass n super exp
    VObject (Pid pid) -> sendM pid (Eval exp) >> return cls
 			   
eval exp = throwError $ "Cannot yet evaluate the following expression:\n" ++ show exp


-- | Eval with tail calls
--   Like @eval@ but places the result in the the response of the continuation
--   rather then return them as the result.  This allows for proper tail 
--   recursion.  Most cases are handled by calling back to eval, but those
--   that are suseptable to tail recursion are reimplimented here.
evalT ::  Exp -> EvalM ()
evalT (Apply var args) = do
  (fn, arity) <- fnFromVar var
  guard $ checkArity arity $ length args
  vals <- mapM eval args
  fn vals
evalT (Call  expr msg args) = do
  val <- eval expr
  r <- liftIO $ valToObj val
  vals <- mapM eval args
  case r of 
    Pid pid -> tailM pid (Execute (simple msg) vals)
    receiver -> fmap fst $ with receiver $ do 
      (method, arity) <- fnFromVar (simple msg)
      guard $ checkArity arity $ length vals
      method vals
    -- TODO put self back (see issue #28 on github)
evalT (If pred cons alt) = do
  r <- eval pred
  if r == VNil || r == VFalse then maybe (replyM_ VNil) evalT alt else evalT cons
evalT exp = eval exp >>= replyM_  -- General case


fnFromVar :: Var -> EvalM (Fn, Arity)
fnFromVar var = do
  val <- lookupVar var
  case val of
    (VFunction fn arity) -> return (fn, arity)
    (VError _) -> throwError $ "Function or Method not found: " ++ show var
    val -> return (\vals -> evalT (Call (EValue val) "call" (map EValue vals)), (1,Nothing))

buildClass n super exp = do
  VObject superClass <- eval (EVar $ maybe (simple "Object") id super)
  VObject clsClass <- eval (EVar $ simple "Class")
  let cls = 
        VObject Class 
          { ivars = M.empty
	  , klass = clsClass
	  , modules=[]
	  , process = Nothing
	  , super = superClass
	  , cvars = M.empty
	  , properName = name n
	  }
  Pid pid <- liftIO $ spawn cls
  sendM pid $ Eval exp
  eval $ Call (EVar Var{name="Object", scope=[]}) "setCVar" [EAtom $ name n, EValue $ VObject $ Pid pid]
  return $ VObject $ Pid pid

mkFunct :: [String] -> Exp -> [Value] -> EvalM ()
mkFunct params exp vals = do
  c <- get
  using (merge (zip params vals) c) (evalT exp) -- Use evalT for proper tail calls

using :: Context -> EvalM a -> EvalM a
using c evalm = do
  cOld <- get
  put c
  resp <- evalm
  slf <- gets self
  put cOld{self = (slf)}
  return resp

-- Impliments the Shunting-yard Algorithm
-- of Edsger Dijstra as described at
-- http://www.wcipeg.com/wiki/Shunting_yard_algorithm
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

