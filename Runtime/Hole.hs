module Runtime.Hole
  ( Hole
  , mkHole
  , readHole
  , maybeReadHole
  , writeHole) where

import Control.Concurrent.STM

newtype Hole a = Hole {unHole :: TMVar a}

mkHole :: STM(Hole a)
mkHole = Hole <$> newEmptyTMVar

readHole :: Hole a -> STM(a)
readHole (Hole tmv) = readTMVar tmv

maybeReadHole :: a -> Hole a -> STM a
maybeReadHole alt (Hole tmv) = do
  res <- tryReadTMVar tmv
  case res of
    Just a -> return a
    Nothing -> return alt

writeHole :: Hole a -> a ->STM Bool
writeHole (Hole tmv) x = tryPutTMVar tmv x
