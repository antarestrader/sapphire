module Test.Array where

import Test.QuickCheck
import Array
import Object

-- An empty array returns nil for every index
prop_NilOnEmpty (Positive i) = (empty 37) ! i == VNil

-- A value put in can be retrieved
prop_InAndOut v (Positive i)   = (insert (empty 37) i v) ! i == v
