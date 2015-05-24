-- Array/BuiltinFunctions.hs Copyright 2014 John F. Miller

-- | This module exports the functions needed to bootstrap the Array Class.

module Builtin.Array where

import Prelude hiding (length)
import qualified Array as A
import Array (Array,(|>),(><),(<|))
import qualified Data.Map as M
import Object
import Eval
import AST
import Err
import Control.Monad.Except
import Builtin.Utils
import Context (replyM_)
import Data.Foldable


bootstrap = M.fromList [
         ("length" , VFunction length (0,Just 0))
       , ("__index", VFunction index (1,Just 1))
       , ("inject" , VFunction inject (2,Just 2))
       , ("push"   , VFunction push (0,Nothing))
       , ("concat" , VFunction concatFn (1,Just 1))
       , ("to_arr" , VFunction toArrFn (0, Just 0))
       , ("empty?"  , VFunction emptyFn (0, Just 0))
       ]

-- | build the Array class with internal functions installed
arrayClass :: EvalM Object
arrayClass = buildClass "Array" bootstrap

emptyFn _ = do
  arr <- toArray =<< innerValue
  replyM_ $ vbool $ A.null arr

length :: [Value] -> EvalM ()
length [] = do
  (VArray arr) <- innerValue -- TODO: deal with match failure
  replyM_ $ VInt $ fromIntegral $ A.length arr

index [VInt i] | i>=0 = do
    (VArray arr) <- innerValue
    if i' >= A.length arr
      then replyM_ VNil -- out of bounds
      else replyM_ $ arr `A.index` i'
  where i' = fromInteger i
index _ = error "Index must take a non-negative integer, Call '[]' instead."

inject [init, fn] = do
  arr <- toArray =<< innerValue
  replyM_ =<< foldlM fn' init arr
    where
      fn' accum val = eval (ApplyFn (EValue fn) [(EValue accum),(EValue val)])

push [] = replyM_ =<< innerValue
push [val] = do
  arr <- toArray =<< innerValue
  replyM_ $ VArray (arr |> val)
push vals = do
  arr <- toArray =<< innerValue
  replyM_ $ VArray (arr >< A.fromList vals)

concatFn (other:_) = do
  fst <- toArray =<< innerValue
  snd <- toArray other
  replyM_ $ VArray (fst >< snd)

toArrFn _ = replyM_ =<< innerValue

toArray :: Value -> EvalM Array
toArray val = loop val 12
  where
    loop (VArray arr) _ = return arr
    loop _ 0 = throwError $ Err "RunTimeError" "Value refuses to be converted to a Array" [val]
    loop val' n = do
      val'' <- eval (Call (EValue val') "to_arr" [])
      loop val'' (n-1)
