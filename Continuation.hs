module Continuation 
  ( Message
  , MessageQueue
  , ProcessId
  , ContM
  , Responder
  , Continuation
  , newContIO
  , newCont
  , send
  , dispatch
  
  )where

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar 
import Control.Concurrent.STM.TChan 

type Message m r = (m, Continuation m r)

newtype MessageQueue m r = Queue (TChan (Message m r))

type ProcessId m r = (ThreadId, MessageQueue m r)

type ContList m r = [ProcessId m r]

type ContM m r = StateT (Continuation m r) STM

type Responder o m r = o -> Message m r -> IO o

data Continuation m r = 
    Cont 
      {
	replier   :: TMVar r
      , receivers :: ContList m r
      }

newContIO ::  IO (Continuation m r)
newContIO = do
  x <- newEmptyTMVarIO
  return $ Cont { replier = x, receivers=emptyContList}

newCont ::  ContM m r (Continuation m r)
newCont = do
  x <- lift newEmptyTMVar
  return $ Cont { replier = x, receivers=emptyContList}

spawnCont :: ProcessId m r -> ContM m r (Continuation m r)
spawnCont pid = do
  cont <- get
  x <- lift newEmptyTMVar
  return $ cont{ replier = x, receivers = insertContList pid (receivers cont)}
  
runContM ::  Continuation m r -> ContM m r a -> IO a
runContM cont comp = atomically $ evalStateT comp cont

-- send a message, but don't wait for the response
send :: Continuation m r -> ProcessId m r -> m -> IO ()
send cont (tid, chan) msg = do
  let Queue chan' = maybe chan id $ findChannel tid $ receivers cont
  cont' <- newContIO
  atomically $  writeTChan chan' (msg, cont') 

-- send a message ans wait for the response 
dispatch :: Continuation m r -> a -> Responder a m r -> ProcessId m r-> m -> IO r
dispatch cont obj resp (tid, chan) msg = do
  rset <- runContM cont $ do
      Queue dispatchChan <- gets $ maybe chan id . findChannel tid . receivers -- find the correct channel
      rChan <- lift $ newTChan -- prepair a new subchannel
      let pid = (tid, Queue rChan) 
      cont' <- spawnCont pid -- continuation with the new subchannel and fresh replier
      lift $ writeTChan dispatchChan (msg, cont') -- send the message
      return (rChan, replier cont') 
  loop rset obj 
    where
      -- loop ::(TChan (Message m r), TMVar r) -> a -> IO r
      loop rr@(responseChan, reply) obj' = do 
        r <- atomically ((readTChan responseChan >>= (return . Left))  `orElse` (readTMVar reply >>= (return . Right)))
        case r of
          Left m -> resp obj' m >>= loop rr
          Right r' -> return r' 

-- cont list may become smarter (i.e Data.Map)

emptyContList :: ContList m r
emptyContList = []

insertContList :: ProcessId m r -> ContList m r -> ContList m r
insertContList = (:) -- may need to eliminate older responders(?)

findChannel :: ThreadId -> ContList m r-> Maybe (MessageQueue m r)
findChannel = lookup
