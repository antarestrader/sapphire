module Builtin.Atom where

import qualified Data.Map as M
import String
import Object
import Eval
import AST
import Builtin.Utils
import Context (replyM_)

atomClass = buildClass "Atom" bootstrap

bootstrap =  M.fromList [
      ("=="     , VFunction eq   (1,Just 2))
    , ("to_s"   , VFunction to_s (0,Just 0))
    ]

eq :: [Value] -> EvalM ()
eq ((VAtom s):_) = do
  (VAtom t) <- innerValue
  if s == t then replyM_ VTrue else replyM_ VFalse

to_s :: [Value] -> EvalM ()
to_s _ = do
  (VAtom slf) <- innerValue
  replyM_ $ VString $ mkStringLiteral slf

