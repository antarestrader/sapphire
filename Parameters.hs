-- Copyright 2017 John F. Miller

-- | Datatype for describing the formal parameters of a Sapphire function.
--
-- ==Examples
--
-- > def foo(bar, baz = 5, *qaax)
--
-- @bar@ is a normal required parameter.
--
-- @baz@ is an optional parameter with a default value of 5.
--
-- @qaax@ will be a (possibally empty) array with all remaining arguments.
module Parameters (
    Parameter(..)
  )
where

import Data.List

import {-# SOURCE #-} Object hiding (arity)
import Name

-- | Datatype for describing the formal parameters of a Sapphire function.
data Parameter = 
    P Name Parameter -- ^ Normal named parameter
  | Empty -- ^ No more parameters
  | Alternatives [Parameter] -- ^ Alternatives for poor man's pattern match
  | Asterisk Name -- ^ Group all remaining arguments into an array
  | Default Name Object Parameter -- ^ A parameter with a default value 
  | Pattern Name Name Parameter 
    -- ^ Require the arg match class Name to succeed.
  | Guard Fn Parameter -- ^ __Expiramental__: require Fn be true to succeed

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




