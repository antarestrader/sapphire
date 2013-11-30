module Eval where

import Data.ByteString
import qualified Data.Map as M
import Parser

eval :: Context -> Exp -> Value
eval _ (EInt i) = VInt i
eval _ (EFloat f) = VFloat f
eval _ ENil = VNil

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

data SapString = SapString {encoding :: String, esscapes :: [String], bytes :: ByteString} --FixMe

type Pid = Integer

data Object = Object {vals :: M.Map String Value}

type Context = M.Map String Value
