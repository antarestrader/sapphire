-- Copyright John F. Miller 2017

-- | A Hole is a TMVar that can only ever be written to once.
module Runtime.Hole
  ( Hole
  , mkHole
  , readHole
  , maybeReadHole
  , writeHole) where

import Control.Concurrent.STM

-- | a STM location that can be written once then read as many times as
--   needed.
newtype Hole a = Hole {unHole :: TMVar a}

-- | Make a new empty Hole
mkHole :: STM(Hole a)
mkHole = Hole <$> newEmptyTMVar

-- | Read the contents of a Hole, block if the hole is empty
readHole :: Hole a -> STM(a)
readHole (Hole tmv) = readTMVar tmv

-- | If the Hole is full returns its value, otherwise returns the first
--   argument.
maybeReadHole :: a -> Hole a -> STM a
maybeReadHole alt (Hole tmv) = do
  res <- tryReadTMVar tmv
  case res of
    Just a -> return a
    Nothing -> return alt

-- | Write to an empty hole. If the hole is already full just return false
--   without changing the content of the hole. The return valuse is typically
--   ignored.
writeHole :: Hole a -> a ->STM Bool
writeHole (Hole tmv) x = tryPutTMVar tmv x
