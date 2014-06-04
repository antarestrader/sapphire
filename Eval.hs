-- Copyright 2013, 2014 John F. Miller
{-# LANGUAGE ScopedTypeVariables #-}

-- | Evaluate the Abstract Syntax Tree expression to produce values within the
--   the context of the EvalM Monad
--
--  In sapphire this is accomplished by a rather bastardized version of
--  continuation passing style.  In our case continuations are a datastructure
--  (found in Continuation.hs) that holds a (TMVar Value).  This is the
--  equivelent of the return site in traditionla CPS.
--
--  In order for this to actually be effective, we needed to change function
--  application from a function takeing a list of values and returning a
--  Monadic value result to a function taking a list of values that has re
--  effect (monadic action) of placing a value in the replier TMVar in the
--  current context.
--
--  In point of fact most of `eval` is simple translation from Exp to Value.
--  There are also occasions when we need an actual value to be returned.  for
--  these reasons, eval was not replaced, but suplimented wiht the evalT
--  function which can be called whenever it is appropriate to reply with the
--  resulting value rather then retuning it to the calling function.  The
--  default implimentation of this function is to call eval and place the
--  resulting into the replier of context.  For expressions such as apply and
--  call where proper tail calls are in order evalT reimpliments them.

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
import Data.Monoid
import AST
import Object
import Object.Graph
import Object.Spawn
import String
import Array hiding (length, map)
import Context
import Var
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Exception(try, BlockedIndefinitelyOnSTM)

type Err = String

-- | The moand in which Sapphire code runs.  It contains the Context, handles
--   errors and allows for IO actions in the running program.
type EvalM a= StateT Context (ErrorT Err IO) a

-- | execute the EvalM action using the provided Context.
runEvalM :: (EvalM a) -- ^ the action to be run 
         -> Context   -- ^ the context to run it in
         -> IO (Either Err (a, Context))
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

eval (EString s) = return $ VString $ mkStringLiteral s

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
  r <-  valToObj val
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
    VObject (Pid pid) -> sendM pid (Eval exp) >> return cls
    _ -> buildClass n super exp

eval (ExString xs) = do
    vals <- mapM eval xs
    concatenate [] vals
  where
    concatenate :: [SapString] -> [Value] -> EvalM Value
    concatenate ss [] = return $ VString $ mconcat (reverse ss)
    concatenate ss (v:vs) = do { s<-vToStr v ; concatenate (s:ss) vs }

    vToStr :: Value -> EvalM SapString
    vToStr (VString s) = return s
    vToStr val = do
      v' <- eval (Call (EValue val) "to_s" [])
      vToStr v'

eval (EArray xs) = do 
  x's <-  mapM eval xs
  return $ VArray $ fromList x's

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
  r <- valToObj val
  vals <- mapM eval args
  case r of 
    Pid pid -> tailM pid (Execute (simple msg) vals)
    receiver -> fmap fst $ with receiver $ do 
      (method, arity) <- fnFromVar (simple msg)
      guard $ checkArity arity $ length vals
      method vals
    -- TODO put self back (see issue #28 on github)
evalT (Block exps) = mapM_ eval (init exps) >> evalT (last exps) 
evalT (If pred cons alt) = do
  r <- eval pred
  if r == VNil || r == VFalse then maybe (replyM_ VNil) evalT alt else evalT cons
evalT exp = eval exp >>= replyM_  -- General case

-- | Given a Var find or create a function to call
--
-- In the most obvios case, we look up the Var in the current context and
-- find a function value which we shall return directly.
--
-- In the case of an error we propigate the not found signal and move on.
--
-- When we encounter a non-function value, we create a temporary function
-- which uses the "call" method.  By implimenting this method, any object
-- can pretend to be a function.
fnFromVar :: Var -> EvalM (Fn, Arity)
fnFromVar var = do
  val <- lookupVar var
  case val of
    (VFunction fn arity) -> return (fn, arity)
    (VError _) -> throwError $ "Function or Method not found: " ++ show var
    val -> return (\vals -> evalT (Call (EValue val) "call" (map EValue vals)), (1,Nothing))

-- | The inner workings of Class creation
--
-- If a super class is not given we assume Object by default; every class is an
-- instance of Classs. The class structure is built and a new class is spawned.
-- Becase classes are by defination shared information among all their 
-- instances, every class must have its own process.
--
-- All classes are registered by their proper name in Object or the containing
-- module once implimented.
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

-- | The internal working so making a function
mkFunct :: [String]  -- formal parameters (TODO improve see issue #29)
        -> Exp       -- the expression to be evaluated (typically a block)
        -> [Value]   -- the actual parameters
        -> EvalM ()  -- the resulting value is placed in the replier
mkFunct params exp vals = do
  c <- get
  using (merge (zip params vals) c) (evalT exp) -- Use evalT for proper tail calls

-- | Part of the process of applying a function is to set the formal parameters
--   equal to the actual parameters.  Once accomplished, a new context is
--   created.  This function allows that new context to be safely used and then
--   discarded.
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
