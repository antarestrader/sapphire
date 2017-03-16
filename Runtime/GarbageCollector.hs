module Runtime.GarbageCollector ( 
    GC, newGC, register, rootSet, garbageCollector
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable (foldrM)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Text.Printf

import Runtime.PID

type GC obj = TQueue (GCMessage obj)
type GCResponse a =  TQueue (ThreadId,[PID a])
type GCSet a =  Map ThreadId (GCState a)
data GCMessage a = Register (PID a) | LiveSet [PID a] | RunNow
data GCState a = Dead (PID a) | Live (PID a) Bool

newGC :: IO(GC a)
newGC = do
    q <- newTQueueIO
    forkIO $ loop q [] [] 256
    return q
  where
   loop :: GC a -> [PID a] -> [PID a]-> Int -> IO()
   loop q set root n = do
     m <- atomically $ readTQueue q
     case m of
       Register pid -> do
         let set' = (pid:set)
         if (length set') > 2*n
           then markAndSweep set root >>= (\s -> loop q s root (length s))
           else loop q set' root n
       LiveSet ps -> loop q set (root++ps) n
       RunNow -> markAndSweep set root >>= (\s -> loop q s root (length s))

markAndSweep :: [PID a] -> [PID a] -> IO([PID a])
markAndSweep ps [] = do
  atomically$ mapM_ (\p -> writePID p Quit) ps
  return []
markAndSweep ps root = do
    r <- newTQueueIO :: IO (GCResponse a)
    let set' = M.fromList (map (\p -> (tID p, Dead p )) ps) 
    set <- atomically $ mark r root set'
    loop r set
 where
   loop :: GCResponse a -> GCSet a -> IO ([PID a])
   loop r set = case any pending set of
     False -> do  -- All live processes have reported in 
       let (d,l) = M.partition dead set
       atomically $ mapM_ (\x -> writePID (pid x) Quit) (M.elems d)
       printf "GC Run: %i dead, %i alive\n" (length d) (length l) 
       return $ map (pid) (M.elems l)
     True  -> do  -- Wait for processes to return
       set'' <- atomically $ do
         (tid,ps) <- readTQueue r
         let set'  = M.adjust (\x -> Live (pid x) False) tid set
         mark r ps set'
       loop r set''
   pending (Live _ x) = x
   pending _ = False
   dead (Dead _) = True
   dead _ = False
   pid (Dead p) = p
   pid (Live p _) = p
   mark :: GCResponse a -> [PID a] -> GCSet a -> STM (GCSet a)
   mark r ps set = foldrM mark' set ps
     where
       mark' p set = case M.lookup (tID p) set of
         Just (Dead _) -> do
            writePID p $ Mark (\ps -> myThreadId >>=(\t -> (atomically $ writeTQueue r (t,ps))))
            return $ M.insert (tID p) (Live p True) set
         otherwise -> return set

register :: GC a-> PID a -> IO()
register gc pid = atomically $ writeTQueue gc $ Register pid

rootSet  :: GC a-> [PID a] -> IO()
rootSet gc ps = atomically $ writeTQueue gc $ LiveSet ps

garbageCollector gc = atomically $ writeTQueue gc $ RunNow
