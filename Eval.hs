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
import Data.Foldable (toList)
import Data.Maybe
import Data.Monoid
import AST
import Err
import Object
import Object.Graph
import Object.Spawn
import String
import qualified Array as A
import Hash
import Builtin.Hash
import Context hiding (scope, global)
import qualified Context as CTX
import Var
import Utils
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Exception(try, BlockedIndefinitelyOnSTM)
import Control.Concurrent.STM.TMVar

-- | The moand in which Sapphire code runs.  It contains the Context, handles
--   errors and allows for IO actions in the running program.
type EvalM a= StateT Context (ExceptT (Err Value) IO) a

-- | execute the EvalM action using the provided Context.
runEvalM :: (EvalM a) -- ^ the action to be run
         -> Context   -- ^ the context to run it in
         -> IO (Either (Err Value) (a, Context))
runEvalM e c = do
  r <- try $ runExceptT $ runStateT e c
  case r of
    Right x -> return x
    Left (e:: BlockedIndefinitelyOnSTM) -> return $ Left $ Err "ConcurencyError" (show e) []

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
  (MV _ val) <- lookupIVar s
  return val

eval (EVar var) = do
  (MV _ val) <- lookupVar var
  return val

eval (OpStr a ops) = do
  c <- get
  eval (shunt (precedence c) [a] [] ops)

eval (Assign lhs exp) = do
  val <- eval exp
  insertLHS lhs val
  return val

eval (Call expr msg args) = do
  (MV f val) <- evalWithContext expr
  r <-  valToObj val
  case r of
    Pid pid -> do
      vals <- mapM eval args
      (fmap responseToValue) $ dispatchM pid (Execute (simple msg) vals)
    receiver -> do
      args' <- mapM (liftM EValue . eval) args
      (result, obj') <- with receiver $ eval (Apply (simple msg) args' Public)
      f $ VObject obj' -- update self
      return result

eval (Index expr args) = do
  val <- eval expr
  idxs <- mapM eval args
  case (val,idxs) of
    (VArray a, [VInt i]) | i >= 0 -> return (if ((fromInteger i) < A.length a) then a `A.index` fromInteger i else VNil)
    (v,xs) -> eval (Call (EValue v) "[]" $ map EValue xs)

eval ast@(Apply _ _ _) = (fmap responseToValue) $ extract $ evalT ast

eval (ApplyFn fnExp args) = do
  fn <- eval fnExp
  (fn,arity) <- mkFunctionFromValue (simple "<an expression>") fn
  (fmap responseToValue) $ extract $ apply fn arity emptySuper args

eval spr@(ESuper _) = (fmap responseToValue) $ extract $ evalT spr

eval (Def n params exp) = eval $ Assign (LCVar n) (Lambda params exp)

eval (DefSelf n ps exp) = do
  (mdl,fn) <- localModule
  (val,mdl') <- with mdl $ eval $  Assign (LCVar n) (Lambda ps exp)
  fn mdl'
  return val

eval (Lambda params exp) = return (VFunction (mkFunct params exp) (mkArity params))

eval (Block exps file) = do
  file' <- gets $ lookupLocals "__FILE__"
  insertLocalM "__FILE__" $ VString $ mkStringLiteral file
  v <- fmap last $ mapM eval exps
  insertLocalM "__FILE__" $ maybe VNil id file'
  return v

eval (If pred cons alt) = do
  r <- eval pred
  if r == VNil || r == VFalse then maybe (return VNil) eval alt else eval cons

eval (While cond exp) =
  let 
    loop last = do
      cond' <- eval cond
      case cond' of
        (VNil) -> return last
        (VFalse) -> return last
        _ -> loop =<< eval exp
  in
    loop VNil

eval (EClass Self _ exp) = do
  (mdl, update) <- localModule
  (val,mdl')<- with mdl $ eval exp
  update mdl'
  return val

eval (EClass n super exp) = do
  cls <- eval (EVar n)
  case cls of
    VObject (Pid pid) -> sendM pid (Eval exp) >> return cls
    _ -> buildClass n super exp

eval (Module n exp) = do
  mdl <- eval (EVar n)
  case mdl of
    VObject (Pid pid) -> sendM pid (Eval exp) >> return mdl
    _ -> buildModule n exp

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

eval exp = throwError $ Err "SystemError" ("Cannot yet evaluate the following expression:\n" ++ show exp) []

-- | Eval with context update
--   When an expression could destructivally modify a sub-expression, it is
--   necessary to provide a way to modify the context with the new value.  The
--   second return value is a function that will replace the value of the evaluated
--   object with the given value.

evalWithContext :: Exp -> EvalM (MutableValue)
evalWithContext (EIVar str) = lookupIVar str
evalWithContext (EVar var) = lookupVar var
-- TODO: evalWithContext (Call exp str args)
evalWithContext exp = fmap (MV (\_->return ())) (eval exp)


-- | Eval with tail calls
--   Like @eval@ but places the result in the the response of the continuation
--   rather then return them as the result.  This allows for proper tail
--   recursion.  Most cases are handled by calling back to eval, but those
--   that are suseptable to tail recursion are reimplimented here.
evalT ::  Exp -> EvalM ()
evalT (Apply var args vis) = do
  (fn, arity, super, args') <- let
                          handler (Err "NotFoundError" msg []) =
                            case var of
                              Var{name = str, scope =[]} -> do
                                (f,a,s) <- fnFromVar (simple "method_missing") Public
                                return (f,a,s,((EAtom str):args))
                              _ -> do
                                slf <- gets self
                                throwError $ Err "MethodMissing" msg [VObject slf]
                          handler err = throwError err

                          find = do
                            (f,a,s) <- fnFromVar var vis
                            return (f,a,s,args)

                        in
                          find `catchError` handler
  apply fn arity super args'
evalT (Call expr msg args) = do
  val <- eval expr
  r <- valToObj val
  case r of
    Pid pid -> do
      vals <- mapM eval args
      tailM pid (Execute (simple msg) vals)
    receiver -> do
      args' <- mapM (liftM EValue . eval) args
      fmap fst $ with receiver $ evalT (Apply (simple msg) args' Public)
    -- TODO: put self back (see issue #28 on github)

evalT (Block exps file) = do
  insertLocalM "__FILE__" $ VString $ mkStringLiteral file
  mapM_ eval (init exps) >> evalT (last exps)

evalT (If pred cons alt) = do
  r <- eval pred
  if r == VNil || r == VFalse then maybe (replyM_ VNil) evalT alt else evalT cons

evalT (ESuper m_args) = do
  spr <- join $ gets superMethod
  case spr of
    Nothing -> throwError $ Err "NoMethodError" "super method not found" []
    Just mws -> do 
      (fn, arity) <- mkFunctionFromValue (simple "super") $ mwsValue mws
      case m_args of
        Just args -> apply fn arity (runMWS mws) args
        Nothing -> do
          m_args' <- gets $ lookupLocals "__args"
          case m_args' of 
            Nothing -> apply fn arity (runMWS mws) []
            Just (VArray arr) -> apply fn arity (runMWS mws) (map EValue $ toList arr)
            Just (val) -> apply fn arity (runMWS mws) [EValue val]


evalT exp = eval exp >>= replyM_  -- General case

-- | run the function fn with args
apply fn arity super args = do
  guardR "Wrong number of arguments." $ checkArity arity $ length args
  vals <- mapM eval args
  withSuper super $ fn vals

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
fnFromVar :: Var -> Visibility-> EvalM (Fn, Arity, Super)
fnFromVar var Private = do
  MV _ val <- lookupVar var
  (fn,arity) <- mkFunctionFromValue var val
  slf <- gets self
  return (fn, arity, maybe (searchMethods (top var) slf) (const emptySuper) (bottom var))
fnFromVar _ Protected = throwError $ Err "SystemError" "TODO: impliment protected methods" []
fnFromVar var Public = do
  MWS super val <- lookupMethod $ top var
  (fn,arity) <- mkFunctionFromValue var val
  return (fn,arity,super)

-- | Turn a non-function value into a function
mkFunctionFromValue :: Var -> Value -> EvalM(Fn, Arity)
mkFunctionFromValue _ VFunction{function=fn, arity=arity} = return (fn, arity)
mkFunctionFromValue var (VError (Err err msg vals))       = throwError $ Err err (msg ++ "(while looking up function" ++ show var ++ ")") vals
mkFunctionFromValue var VNil                              = throwError $ Err "NotFoundError" ("Function or Method not found: " ++ show var) []
mkFunctionFromValue _ val                                 = return (\vals -> evalT (Call (EValue val) "call" (map EValue vals)), (0,Nothing))

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
  tmvar <- liftIO $ newEmptyTMVarIO
  let cls = Class
          { ivars = M.empty
	  , klass = clsClass
	  , modules=[]
	  , process = tmvar
	  , super = superClass
	  , cvars = M.empty
          , cmodules = []
	  , properName = name n
	  }
  Pid pid <- spawn cls
  sendM pid $ Eval exp
  registerClass n pid

-- | Find or create a module for local method definations.  This function
--   returns both the local module and a function to return that module and
--   a function to place that module backe into self.  Note: it is a bug to
--   either a) modify the list of modules or b) change the meaning of self
--   between the call to localModuled and this function. 
localModule :: EvalM (Object, (Object -> EvalM()))
localModule = do
  slf <- gets self
  case (modules slf) of
    (mdl:_) | (properName mdl == "*") -> return (mdl, fn)
        where fn mdl' = modifySelf (\slf-> slf{modules = (mdl':(tail $ modules slf))})
    _ -> do
      mdl <- newModule "*"
      let fn mdl' = modifySelf (\slf-> slf{modules = (mdl':(modules slf))})
      return (mdl, fn)

newModule str = do
  VObject superClass <- eval (EVar $ simple "Object")
  VObject clsClass   <- eval (EVar $ simple "Module")
  tmvar <- liftIO $ newEmptyTMVarIO
  return $ Class
          { ivars = M.empty
	  , klass = clsClass
	  , modules=[]
	  , process = tmvar
	  , super = superClass
	  , cvars = M.empty
          , cmodules = []
	  , properName = str
	  }

buildModule n exp = do
  mdl <- newModule $ name n
  Pid pid <- spawn mdl
  sendM pid $ Eval exp
  registerClass n pid

registerClass :: Var -> Process -> EvalM Value
registerClass n pid = do
  scp <- gets CTX.scope
  eval $ Call (EValue $ VObject $ scp) "setIVar" [EAtom $ name n, EValue $ VObject $ Pid pid] --fixme: why CVars?

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
    assignParams [] [] = return [("__args",VArray $ A.fromList vals)]
    assignParams [] _ = throwError $ Err "RunTimeError" "Function called with too many arguments" []
    assignParams ((Param str):ps) [] = throwError $ Err "RunTimeError" "Function called with too few arguments" []
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
