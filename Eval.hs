-- Copyright 2013 - 2017 John F. Miller
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings,  NamedFieldPuns #-}

-- | Evaluate the Abstract Syntax Tree expression to produce values within the
--   the context of a Scope Monad.
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
    eval
  , evalT
  )
where

-- import qualified Data.Map as M
import Data.Foldable (toList)
import Data.Maybe
-- import Data.Monoid
import Control.Monad
import Control.Monad.Except
import Control.Exception(try, BlockedIndefinitelyOnSTM)

import AST
import Name
import Scope
import Eval.Parameters as P
import Err
import Object
import String
import qualified Array as A
import Hash
-- import Builtin.Hash
-- import Context hiding (scope, global)
-- import qualified Context as CTX
import Var
import Utils

evalArgs :: Scope m => [Exp] -> m [Object]
evalArgs args = mapM eval args >>= (return . map o)

-- | The internal working so making a function
mkFunct :: Scope m
        => Parameter  -- formal parameters
        -> [Exp]      -- the set of possible expression to be evaluated (typically blocks)
        -> [Object]   -- the actual parameters
        -> m ()
 -- NOTE: Apply is responsible for corectly creating the enclosing scope
mkFunct params expList vals = do
    i <- match params vals
    evalT (expList !! i) -- Use evalT for proper tail calls

promoteError :: Scope m => Position -> Object -> m (Value m)
promoteError _ e@(VError (ErrPos _ _ _ _)) = throwError e
promoteError p (VError (Err cls txt xs)) = throwError $ VError $ ErrPos cls txt p xs
promoteError p (Prim (VString str)) = case split str of
    (str',"") -> throwError $ VError $ ErrPos "StandardError" str' p []
    (cls,txt ) -> throwError $ VError $ ErrPos cls (tail txt) p []
  where
    split str = break (==':') (string str)
promoteErrot e = throwError e

-- |Turns an expression into a Value, potentially performing
--  side effects along the way.
eval :: Scope m => Exp -> m (Value m)
eval Exp{node,position} =
  eval' node `catchError` promoteError position

eval' :: Scope m => Node -> m (Value m)
eval' (EVar Self) = self
eval' (EVar Var {name, varscope = []}) =  maybe (v Nil) id <$> readVar Local name
eval' (EVar var) = maybe (v Nil) id <$> findVar var
eval' (EInt i) = return $ v (Prim $ VInt i)
eval' (EFloat f) = return $ v (Prim $ VFloat f)
eval' (EString s) = return $ v $ Prim $ VString $ fromString s
eval' (ExString xs) = do
    vals <- evalArgs xs
    concatenate vals ""
  where
    concatenate :: Scope m => [Object] -> SapString -> m (Value m)
    concatenate [] ss = return $ v $ Prim $ VString ss
    concatenate (v:vs) ss = do
        s  <-vToStr v
        ss' <- ss `sconcat` s
        concatenate vs ss'

    vToStr :: Scope m => Object -> m SapString
    vToStr (Prim (VString s)) = return s
    vToStr obj = call (Just $ v obj) "to_s" [] >>= vToStr . o

eval' (EAtom a) = return $ v (Prim $ VAtom a)
eval' (EArray []) = return $ v $ Prim $ VArray $ A.empty
eval' (EArray xs) = do
  x's <-  evalArgs xs
  return $ v $ A.varray x's

eval' (EHash xs) = do
  let f (a,b) = do
        a' <- o <$> eval a
        b' <- o <$> eval b
        return (a',b')
  x's <- mapM f xs
  return $ v $ vhash x's

eval' (EIVar name) =  maybe (v Nil) id <$> readVar IVar name
eval' ENil = return (v Nil)
eval' ETrue = return (v TrueClass)
eval' EFalse = return (v FalseClass)
eval' (OpStr a ops) = do
  pt <- presidenceTable
  eval (shunt pt [a] [] ops)

eval' (Index expr args) = do
  val <- eval expr
  idxs <- evalArgs args
  case (o val,idxs) of
    (Prim (VArray a), [Prim (VInt i)]) | i >= 0 ->
        return $ v (
            if (fromInteger i) < A.length a
              then a `A.index` fromInteger i
              else Nil
          )
    (_,xs) -> call (Just val) "[]" xs

eval' (Lambda params exp) = do
  uid <- nextUID
  return $ v
    ( VFunction
        { function = AST params exp
        , Object.arity = P.arity params
        , fUID = uid
        }
    )

eval' (Def vis ord name params exps) =
  defMethod vis ord name params exps >> (return $ v Nil)

 -- this can mean three things
 --  1) this is a local method call; call it
 --  2) this is a Lambda function; apply it
 --  3) this is a non-function object;  call the method "call" on it
eval' (Apply  (Var {name, varscope = []}) args vis) = do
 xs <- evalArgs args
 f <- readVar Local name
 case f of
   Nothing -> call Nothing name xs
   Just val ->
     case  o val of
       (VFunction {function = Fn{fn}}) -> newScope $ fn xs
       (VFunction {function = AST{params,asts}}) -> newScope $ mkFunct params asts xs
       otherwise -> call (Just val) "call" xs

eval' (ApplyFn exp args) = do
  val <- eval exp
  xs <- evalArgs args
  case  o val of
    (VFunction {function = Fn{fn}}) -> newScope $ fn xs
    (VFunction {function = AST{params,asts}}) -> newScope $ mkFunct params asts xs
    otherwise -> call (Just val) "call" xs

eval' (Call expr msg args) = do
  target <- eval expr
  xs <- evalArgs args
  call (Just target) msg xs

eval' (Send expr msg args) = do
  -- Can we detect when the return value is not used and avoid the
  -- overhead needed to build a future?
  target <- eval expr
  xs <- evalArgs args
  case target of
    Pointer pid -> do
        future <- future
        send pid msg xs (\obj -> send future "return" [obj] (\_->return ()))
        return $ Pointer future
    val -> call (Just val) msg xs -- Cannot make an async call to a local val.
        -- Question: should this be silent? Warning? Error? option to choose?

eval' (ESuper args) = newScope $ mapM evalArgs args >>= super
eval' (Assign lhs exp) = eval exp >>=(\val -> assign lhs (o val) >> return val)
  where
    assign :: Scope m => LHS -> Object -> m ()
    assign (LVar ( Var {name, varscope = []})) obj = setVar Local name obj
    assign (LIVar name) obj = setVar IVar name obj
    assign (LCVar name) obj = setVar CVar name obj
    assign (LIndex target idxs) obj = do
      args <- (evalArgs idxs) 
      tar <- eval target  
      call (Just tar) "[]=" (args ++ [obj])
      return ()
{-
eval' (OpAssign lhs@(LVar var) op exp) =
  eval' (Assign lhs (OpStr (EVar var) [(op,exp)]))
eval' (OpAssign lhs@(LIVar var) op exp) =
  eval' (Assign lhs (OpStr (EIVar var) [(op,exp)]))
eval' (OpAssign lhs@(LCVar var) op exp) =
  eval' (Assign lhs (OpStr (ECVar var) [(op,exp)]))
-}

eval' (If pred cons alt) = do
  r <- eval pred
  let r' = o r in
    if r' == Nil || r' == FalseClass -- What about processes?
      then maybe (return $ v Nil) eval alt 
      else eval cons

eval' exp =
  throwError $ VError $
    Err  "SystemError"
        ("Cannot yet evaluate the following expression:\n" ++ show exp)
        []

evalT :: Scope m => Exp -> m ()
evalT = undefined


