module Eval where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Parser

eval :: Context -> Exp -> IO (Value, Context)
eval c (EInt i) = return ((VInt i), c)
eval c (EFloat f) = return ((VFloat f),c)
eval c ENil = return (VNil,c)
eval c (EVar Var {name=var}) = case M.lookup var c of  --TODO Scope
  Just val -> return (val, c)
  Nothing  -> do -- TODO Error
    putStrLn $ "Not in scope: " ++ var
    return (VNil, c)
eval c (Assign (LVar Var {name=var}) exp) = do
  (val, c') <- eval c exp
  return (val, M.insert var val c')
eval c exp = do -- TODO Error
  putStrLn "Cannot yet evaluate the following expression"
  print exp
  return (VNil,c)

data Value =
    VInt Integer
  | VFloat Double
  | VString SapString
  | VNil
  | VFunction --FixMe
  | VPid Pid
  | VObject Object

instance Show Value where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VString st) = show $ bytes st
  show VNil = "nil"

data SapString = SapString {encoding :: String, esscapes :: [String], bytes :: B.ByteString} --FixMe

type Pid = Integer

data Object = Object {vals :: M.Map String Value}

type Context = M.Map String Value

emptyContext = M.empty :: Context
