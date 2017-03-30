module Object.UID
  ( UIDSource
  , newUIDSource
  , nextUID
  )
where

import Data.List
import Data.Word
import Data.Bits
import Control.Concurrent.STM

import Name

mixed :: [UID]
mixed = tail $ iterate permutate 0
  where
    permutate :: Word32 -> Word32
    permutate x =  fromInteger (residue (toInteger x)) `xor` 0x5bf03635
    residue :: Integer -> Integer
    residue x = x*x `mod` 4294967291

simple :: [UID]
simple = [1..]

newtype UIDSource = U {unwrap :: TMVar [UID]}

newUIDSource :: IO UIDSource
newUIDSource = U <$> newTMVarIO simple

nextUID :: UIDSource -> STM UID
nextUID src = do
  let tmv = (unwrap src)
  (uid:ids) <- takeTMVar tmv
  putTMVar tmv ids
  return uid

