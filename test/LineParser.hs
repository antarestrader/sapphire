{-# LANGUAGE TypeSynonymInstances #-}
module Test.LineParser where

import LineParser
import Test.QuickCheck

testOffsets = choose (0,100)

prop_noSpace s = not (null s) ==> ((head . line . stripLine 0) s) /= ' '

prop_Offset = (forAll testOffsets (\i ->(offset . stripLine 0) (replicate i ' ' ++ "x\n") == fromIntegral i))


