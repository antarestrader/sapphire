module Builtin.Integer where

import Builtin.Utils
import Context
import Object
import qualified Data.Map as M

integerClass = buildClass "Integer" bootstrap

bootstrap = M.fromList [
    ("+", VFunction add (1,Just 1))
  , ("-", VFunction sub (1,Just 1))
  , ("*", VFunction mul (1,Just 1))
  , ("/", VFunction sapDiv (1,Just 1))
  ]

add [VInt b] = do
  (VInt a) <- innerValue
  replyM_ $ VInt (a+b)

sub [VInt b] = do
  (VInt a) <- innerValue
  replyM_ $ VInt $ a-b

mul [VInt b] = do
  (VInt a) <- innerValue
  replyM_ $ VInt $  a*b

sapDiv [VInt b] = do
  (VInt a) <- innerValue
  replyM_ $ VInt $ a `div` b
