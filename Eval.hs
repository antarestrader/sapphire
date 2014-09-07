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
import qualified Array as A
import Hash
import Hash.BuiltinFunctions
import Context
import Var
import Utils
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
  (val,f) <- evalWithContext expr
  r <-  valToObj val
  vals <- mapM eval args
  case r of 
    Pid pid -> dispatchM pid (Execute (simple msg) vals)
    receiver -> do
      (result, obj') <- with receiver $ do
        (method, arity) <- fnFromVar (simple msg)
        guardR  "Wrong number of arguments." $ checkArity arity $ length vals
        extract $ method vals -- proper tail calls here
      f $ VObject obj' -- update self
      return result

eval (Index expr arg) = do
  val <- eval expr
  idx <- eval arg
  case (val,idx) of
    (VArray a, VInt i) | i >= 0 -> return (if ((fromInteger i) < A.length a) then a `A.index` fromInteger i else VNil)
    (v,i) -> eval (Call (EValue v) "[]" [EValue i])

eval (Apply var args) = do
  (fn, arity) <- fnFromVar var
  guardR "Wrong number of arguments." $ checkArity arity $ length args
  vals <- mapM eval args
  extract $ fn vals -- extract impliments proper tail recursion

eval (ApplyFn fnExp args) = do
  fn <- eval fnExp
  case fn of
    VFunction fn' arity -> do
       guardR "Wrong number of arguments." $ checkArity arity $ length args
       vals <- mapM eval args
       extract $ fn' vals
    obj -> eval (Call (EValue obj) "call" args)

eval (Def n params exp) = eval $ Assign (LCVar n) (Lambda params exp)
eval (Lambda params exp) = return (VFunction (mkFunct params exp) (mkArity params))

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
eval (EArray []) = return $ VArray $ A.empty
eval (EArray xs) = do
  x's <-  mapM eval xs
  return $ VArray $ A.fromList x's

eval (EHash xs) = do
  let f (a,b) = do
        a' <- eval a
        b' <- eval b
        return (a',b')
  x's <- mapM f xs
  hash <- buildHashFromList x's
  return $ VHash hash

eval exp = throwError $ "Cannot yet evaluate the following expression:\n" ++ show exp

-- | Eval with context update
--   When an expression could destructivally modify a sub-expression, it is
--   necessary to provide a way to modify the context with the new value.  The
--   second return value is a function that will replace the value of the evaluated
--   object with the given value.

evalWithContext :: Exp -> EvalM (Value, Value ->EvalM ())
evalWithContext (EIVar str) = do
  val <- gets self >>= lookupIVarsM str
  case val of
    Nothing -> return (VNil, (\_-> return ()))
    Just v -> return (v, (\val -> modifySelf $ insertIVarLocal str val))
evalWithContext (EVar var) = lookupVarContext var
-- TODO: evalWithContext (Call exp str args)
evalWithContext exp = do --by default we do not modify the context
  val <- eval exp
  return (val, \_ -> return ())

-- | Eval with tail calls
--   Like @eval@ but places the result in the the response of the continuation
--   rather then return them as the result.  This allows for proper tail 
--   recursion.  Most cases are handled by calling back to eval, but those
--   that are suseptable to tail recursion are reimplimented here.
evalT ::  Exp -> EvalM ()
evalT (Apply var args) = do
  (fn, arity) <- fnFromVar var
  guardR "Wrong number of arguments." $ checkArity arity $ length args
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
      guardR "Wrong number of arguments." $ checkArity arity $ length vals
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
    VFunction{function=fn, arity=arity} -> return (fn, arity)
    (VError _) -> throwError $ "Function or Method not found: " ++ show var
    val -> return (\vals -> evalT (Call (EValue val) "call" (map EValue vals)), (1,Nothing)) -- fixme

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
mkFunct :: [Parameter]  -- formal parameters (TODO improve see issue #29)
        -> Exp       -- the expression to be evaluated (typically a block)
        -> [Value]   -- the actual parameters
        -> EvalM ()  -- the resulting value is placed in the replier
mkFunct params exp vals = do
    c <- get
    ps <- assignParams params vals
    using (merge ps c) (evalT exp) -- Use evalT for proper tail calls
  where
    assignParams :: [Parameter] -> [Value] -> EvalM [(String,Value)]
    assignParams [] [] = return []
    assignParams [] _ = throwError "Function called with too many arguments"
    assignParams ((Param str):ps) [] = throwError "Function called with too few arguments"
    assignParams ((Default str exp):ps) [] = do
      val <- eval exp
      ps' <- assignParams ps []
      return $ (str, val):ps'
    assignParams [VarArg str] vs = return $ [(str,VArray(A.fromList vs))]
    assignParams (p:ps) (v:vs) = assignParams ps vs >>= (\ps'-> return $ (getParamName p, v):ps') 

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

mkArity :: [Parameter] -> Arity
mkArity params = loop (len,Just len) $ reverse params
  where
    len = length params
    loop a [] = a
    loop (min,_)   ((VarArg _):ps)    = loop (min-1,Nothing) ps
    loop (min,max) ((Default _ _):ps) = loop (min-1,max) ps
    loop a         ((Param _):_)      = a -- halt at the first non-optional param
