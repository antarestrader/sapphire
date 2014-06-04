-- Array.hs Copyright 2014 John F. Miller

module Array where

import {-# SOURCE #-} Object
import Data.Maybe
import Data.Word
import Prelude hiding (map, length)
import qualified Data.IntMap as M
import qualified Data.Array.IArray as A

data Array = Array
  { base   :: A.Array Int Value
  , baseLength :: Int
  , extent :: Int
  , _map    :: M.IntMap Value
  }

empty :: Int -> Array
empty n = Array 
    { base = A.array (0,n-1) (zip [0..n-1] (repeat vnil))
    , baseLength = n-1
    , extent = 0
    , _map = M.empty
    }

(!) :: Array -> Int ->  Value
a ! i | i >= 0 && i < baseLength a = (base a) A.! i
      | i >= baseLength a          = maybe vnil id $ M.lookup i (_map a)
      | -i <= (extent a)           = a ! ((extent a) - i)
      | otherwise                  = verror "Index out of bounds"

insert :: Array -> Int -> Value -> Array
insert a i v |  i >= 0 && i < baseLength a = a{ base = (base a) A.// [(i,v)] }
             |  i >= baseLength a          = a{ _map = M.insert i v (_map a)   }
             |  otherwise                  = error "Index out of bounds"

fromList :: [Value] -> Array
fromList = undefined

slice :: Array -> (Int, Int) -> Array
slice = undefined

length :: Array -> Int
length = undefined

each :: (Monad m) => Array -> (Value -> m ()) -> m ()
each = undefined

map :: (Monad m) => Array -> (Value -> m Value) -> m Array
map = undefined

