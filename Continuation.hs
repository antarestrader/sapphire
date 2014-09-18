-- Continuation.hs Copyright 2013, 2014 John F. Miller

-- | TODO: add overall description

module Continuation
  ( Message
  , MessageQueue
  , ProcessId
  , Responder
  , Replier
  , Continuation (..)
  , newContIO
  , send
  , tail
  , unsafeSend
  , dispatch
  , reply
  , newMessageQueue
  , readQueue
  , respondWith
  , isEmpty
  )where

import Prelude hiding (tail)
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan

-- | A messaged m with a place to sent a reply r
type Message m r = (m, Continuation m r)

-- | A list of messages to be processed (private ?)
newtype MessageQueue m r = Queue (TChan (Message m r))

newMessageQueue :: IO (MessageQueue m r)
newMessageQueue= Queue `fmap` newTChanIO

readQueue :: MessageQueue m r -> IO (Message m r)
readQueue (Queue chan) = atomically $ readTChan chan

writeQueue (Queue chan) = atomically . writeTChan chan

newtype Replier r = R (TMVar r)

-- | A place that messages can be sent
type ProcessId m r = (ThreadId, MessageQueue m r)

-- | a list of Threads with their current message queues
type ContList m r = [ProcessId m r]

-- | A function to process one message and provide a new state
type Responder o m r = o -> Message m r -> IO o

-- | Given and initial state and a Responder creates a process that
--   will respond to messages and keep track of the state
respondWith :: o -> Responder o m r ->  IO (ProcessId m r)
respondWith o r = do
  queue <- newMessageQueue
  tid <- forkIO (loop queue o)
  return (tid, queue)
    where
      loop queue o= do
        m  <- readQueue queue
        o' <- r o m
        loop queue o'  --TODO end loop

-- | The information needed to respond to a message. In order to avoid
--   deadlocks it must also be provided to any down stream messages
data Continuation m r =
    Cont
      {
	replier   :: TMVar r
      , receivers :: ContList m r
      }

-- | A new empty continuation
newContIO ::  IO (Continuation m r)
newContIO = do
  x <- newEmptyTMVarIO
  return $ Cont { replier = x, receivers=emptyContList}

-- | send a message, but don't wait for the response
send :: Continuation m r -> ProcessId m r -> m -> IO (Replier r)
send cont pid msg = do
  let queue = shadowChannel pid cont
  cont' <- newContIO
  writeQueue queue (msg, cont')
  return $ R $ replier cont'

-- | send without checking for shadowing.  This can only be used when the
--   programmer is sure that the process is not shadowed.  E.g. to add an
--   initialization message to a brand new process.
unsafeSend :: ProcessId m r -> m -> IO ()
unsafeSend pid msg = do
  cont' <- newContIO
  writeQueue (snd pid) (msg, cont')

-- | respond to the message with what ever the responce to this call is.
--   proper tail recursion.
tail ::  Continuation m r -> ProcessId m r -> m -> IO ()
tail cont pid msg = do
  let queue = shadowChannel pid cont
  writeQueue queue (msg, cont)


-- | send a message and wait for the response
dispatch :: a -> Responder a m r -> Continuation m r -> ProcessId m r-> m -> IO (r, a)
dispatch obj responder cont pid msg = do
  (cont', responseQueue) <- shadow cont -- set up a shadowed reciever
  let dispatchQueue =  shadowChannel pid cont' -- where to send the message
  writeQueue dispatchQueue (msg, cont') -- send the message
  loop (replier cont') responseQueue obj
    where
      loop answer (Queue chan) obj = do
        r <- atomically ((readTChan chan >>= (return . Left)) `orElse` (readTMVar answer >>= (return . Right)))
        case r of
          Left m -> responder obj m >>= loop answer (Queue chan)
          Right r' -> return (r', obj)

-- | send this reply to the message
reply ::  Continuation m r -> r -> IO Bool
reply cont val= atomically $ tryPutTMVar (replier cont) val

isEmpty :: Continuation m r -> IO Bool
isEmpty cont = atomically $ isEmptyTMVar $ replier cont

-- | cont list may become smarter (i.e Data.Map)
emptyContList :: ContList m r
emptyContList = []

insertContList :: ProcessId m r -> ContList m r -> ContList m r
insertContList = (:) -- may need to eliminate older responders(?)

shadow :: Continuation m r -> IO (Continuation m r, MessageQueue m r)
shadow cont = do
  queue <-  newMessageQueue
  tid <- myThreadId
  rep <- newEmptyTMVarIO
  return (Cont{replier=rep, receivers = insertContList (tid,queue) (receivers cont)}, queue)

shadowChannel :: ProcessId m r -> Continuation m r -> MessageQueue m r
shadowChannel (tid, chan) cont = maybe chan id $ findChannel tid $ receivers cont

findChannel :: ThreadId -> ContList m r-> Maybe (MessageQueue m r)
findChannel = lookup
