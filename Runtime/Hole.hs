module Runtime.Hole (Hole, mkHole, readHole, writeHole) where 

import Control.Concurrent.STM

newtype Hole a = Hole {unHole :: TMVar a}

mkHole :: STM(Hole a)
mkHole = Hole <$> newEmptyTMVar

readHole :: Hole a -> STM(a)
readHole (Hole tmv) = readTMVar tmv

writeHole :: Hole a -> a ->STM Bool
writeHole (Hole tmv) x = tryPutTMVar tmv x
