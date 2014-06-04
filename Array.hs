-- Array.hs Copyright 2014 John F. Miller

module Array where

import {-# SOURCE #-} Object
import Data.Maybe
import Data.Word
import Data.List
import Prelude hiding (map, length)
import qualified Prelude as P
import qualified Data.IntMap as M
import qualified Data.Array.IArray as A

-- | This data structure is intended to provide flexable array like features
--   while maintaining the ability to add elements.  Elements with index less
--   then base length are accessable at O(1) from `base` while those outside of
--   base are O(log n).
data Array = Array
  { base   :: A.Array Int Value -- The basic array
  , baseLength :: Int -- the number of elements in `base`
  , extent :: Int -- the maximum index of the array
  , _map    :: M.IntMap Value -- an efficient map of the remainder of the array
  }

-- | an empty array of length `n`
empty :: Int -> Array
empty n = Array 
    { base = A.array (0,n-1) (zip [0..n-1] (repeat vnil))
    , baseLength = n-1
    , extent = 0
    , _map = M.empty
    }

-- | Element accessor
(!) :: Array -> Int ->  Value
a ! i | i >= 0 && i < baseLength a = (base a) A.! i
      | i >= baseLength a          = maybe vnil id $ M.lookup i (_map a)
      | -i <= (extent a)           = a ! ((extent a) - i)  -- TODO: Debug
      | otherwise                  = verror "Index out of bounds"

insert :: Array -> Int -> Value -> Array
insert a i v |  i >= 0 && i <  baseLength a = a{ base = (base a) A.// [(i,v)] }
             |  i >= baseLength a = a{ _map = M.insert i v (_map a) 
                                               ,  extent = max (extent a) i 
                                               }
             |  otherwise                  = error "Index out of bounds" 

fromList :: [Value] -> Array
fromList [] = empty 20
fromList xs = Array
    { base = A.array (0,l) (zip [0..l] (xs ++ repeat vnil))
    , baseLength = l
    , extent = P.length xs
    , _map = M.empty
    }
  where
    l = max 20 (P.length xs) -- always allocate at least 20 elemets in an array


slice :: Array -> (Int, Int) -> Array
slice = undefined

length :: Array -> Int
length = undefined

each :: (Monad m) => Array -> (Value -> m ()) -> m ()
each = undefined

map :: (Monad m) => Array -> (Value -> m Value) -> m Array
map = undefined

instance Show Array where
  show a = '[':intercalate ", " (P.map show (take (extent a) (A.elems (base a)))) ++ "]"
