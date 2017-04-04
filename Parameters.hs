module Parameters (
    Parameter(..)
  )
where

import Data.List

import {-# SOURCE #-} Object hiding (arity)
import Name

data Parameter = 
    P Name Parameter
  | Empty
  | Alternatives [Parameter]
  | Asterisk Name
  | Default Name Object Parameter
  | Pattern Name Name Parameter
  | Guard Fn Parameter

instance Show Parameter where
  show (Alternatives ps) = intercalate "\n" (map show ps)
  show (Guard _ p) = show p++" | <function>"
  show p = "(" ++ show' p
    where
      show' (P n Empty) = n ++ ")"
      show' (P n x) = n ++ ", " ++ show' x
      show' Empty = ")"
      show' (Asterisk n) = n++"*)"
      show' (Default n obj Empty) = n++"="++show obj++")"
      show' (Default n obj x) = n++"="++show obj++", " ++ show' x
      show' (Pattern cls n Empty) =cls++" "++n++")"
      show' (Pattern cls n x) =cls++" "++n++", " ++ show' x




