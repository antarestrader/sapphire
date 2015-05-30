-- Context.hs  Copyright 2013, 2014 John F. Miller

{-# LANGUAGE FlexibleContexts #-}

-- | When evaluation Sapphire code, this structure keeps track of the context
--   in which the evaluation happens.  More then just a set of local variables,
--   this structure also tracks the current continuation and abstracts the
--   complex rules for sending messages and replying to function found in
--   COntinuations.hs into a more managable framework.
--
--   This module is designed to work primarly with functions who embed a
--   Context structure in a state monad.  The EvalM monad meets this
--   qualification but by making it generic, we ensure easy modifactions to both
--   structures.

module Context
  ( Context( self )
  , modifySelf
  , dispatchC
  , dispatchC_
  , dispatchM
  , sendC
  , sendM
  , tailC, tailM
  , replyM, replyM_
  , with
  , setScope
  , setGlobal
  , newContext
  , newContextIO
  , lookupLocals
  , insertLocals
  , insertLocalM
  , merge
  , hasReply, canReply
  , extract
  , debugM
  ) where

import Prelude hiding (tail)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Object
import Continuation hiding (Responder, Message, Continuation, Replier)

-- | This structure contains the state of a sapphire evaluation.  It is
--   generally assumed that this structure will live in a state monad. In order
--   to enforce a clean interface, only the self field is exposed.
data Context = Context
               { locals :: M.Map String Value
               , self :: Object  -- ^ The object itself
               , scope :: Maybe Object  -- ^ The class / module we are in
               , global :: Object -- ^ the global scope refered to by the prefix scope operator (eg `::Foo`) nominally Object
               , continuation :: Continuation
               , responder :: Responder
               }

newContext obj cont resp = Context
  { locals = M.empty
  , self = obj
  , scope = Nothing
  , global = ROOT
  , continuation = cont
  , responder = resp
  }

newContextIO obj resp = do
  cont <- newContIO
  return $ newContext obj cont resp

-- | Send a process a message and wait for the reply. It is possible that the
--   recieving process will wish to send a message back to the sender (either
--   directly or indirectly) to prevent deadlocks, it must be possible to
--   respond to down stream messages while waiting for the response. This in
--   turn implies that `self` may be modified by such a message in which case
--   the expectation is that the sending process will continue with the
--   modified version.  This function therefore requires the current context,
--   and returns a possibilly modified version of that context along with the
--   response value.
dispatchC :: Context -- ^ the current Context
          -> Process -- ^ the process to send the message to
          -> Message -- ^ The message to be sent
          -> IO (Response, Context) -- ^ the responce to the message, the updated context
dispatchC c pid msg= do
  (val, self') <- dispatch (self c) (responder c) (continuation c) pid msg
  return (val, c{self=self'})

-- | Like dispatchC above, but ignores any changes made to the object itself.
--   This is primarly used by search functions who can ensure that they are
--   read-only all the way down the line.
dispatchC_ :: Context -> Process -> Message -> IO Response
dispatchC_ c p m = fst `fmap` dispatchC c p m

-- | Send a message to a process and block until it responds.
dispatchM :: (MonadState Context m, MonadIO m) => Process -> Message -> m Response
dispatchM pid msg= do
  context <- get
  (val,context') <- liftIO $ dispatchC context pid msg
  put context'
  return val

-- | Send a message to a proces but do not wait for the response.
sendC :: Context->Process->Message-> IO Replier
sendC c pid msg = send (continuation c) pid msg

-- | send a message but do not wait for a response
sendM ::  (MonadState Context m, MonadIO m) => Process -> Message -> m Replier
sendM pid msg = do
  cont <- gets continuation
  liftIO $ send cont pid msg

-- | Respond to our caller with the result of this call.  I.e. we send
--   the return address we got on to this function rather then create one
--   to listen to ourselves.
--   Proper tail recursion.
tailC :: Context->Process->Message-> IO ()
tailC c pid msg = tail (continuation c) pid msg

-- | Same as @tailC@ but with the context embedded in a state monad
tailM :: (MonadState Context m, MonadIO m) => Process -> Message -> m ()
tailM pid msg = do
  cont <- gets continuation
  liftIO $ tail cont pid msg

-- | Send a response to the calling process if no response has yet been given.
--   Returns true if the response was sent, false if a previous value had been
--   placed in the reply.
replyM ::  (MonadState Context m, MonadIO m) => Value -> m Bool
replyM val = do
  cont <- gets continuation
  liftIO $ reply cont (Response val)

-- | Send a response to the calling process like replyM, but with a unit return.
replyM_ val = replyM val >> return ()

-- | True if a reply has been given already, false if a reply is still needed
hasReply :: (MonadState Context m, MonadIO m) => m Bool
hasReply = gets continuation >>= liftIO . isEmpty

-- | The logical opposite of hasReply.  True if a reply is still needed, false
--   if one has already been given.
canReply :: (MonadState Context m, MonadIO m, Functor m) => m Bool
canReply = not `fmap` hasReply

-- | Given an action that sets a reply value, run that action capturing the
--   reply value.  This replaces the replier with a temporary one so the reply
--   is not propigated to the outside context.
--
-- Note to self: looking at this code later I'm not sure it is all completely thought through
-- if there is a deadlock bug, look here.
extract :: (MonadState Context m, MonadIO m) => m () -> m Response
extract act = do
  Context{continuation = cont} <- get
  r <- liftIO newEmptyTMVarIO
  modify (\ctx -> ctx{continuation = cont{replier = r}})
  act
  modify (\ctx -> ctx{continuation = cont}) -- return the old continuation
  liftIO $ atomically $ readTMVar r

-- | Evaluate a method with the given object as self.  Returns both the result
--   and a potentially modified version of the object. Any other changes to the
--   context (ie scope and global) are discarded.
with ::  (MonadState Context m, MonadIO m) => Object -> m a -> m(a, Object)
with obj f = do
  ctx <- get
  put ctx{self= obj}
  a <- f
  obj' <- gets self
  put ctx
  return (a, obj')

-- | update the @self@ object of the current context
modifySelf ::  (MonadState Context m) => (Object -> Object) -> m ()
modifySelf f = modify (\context -> context{self = f $ self context})

setScope :: (MonadState Context m) => Object  -> m ()
setScope cls = modify (\context -> context{scope = Just cls})

setGlobal cls = modify (\context -> context{global = cls})

lookupLocals :: String -> Context -> Maybe Value
lookupLocals s Context{locals = l} = M.lookup s l

-- | Insert a value into the local namespace
insertLocals :: String -> Value -> Context -> Context
insertLocals s val c@Context {locals=l} =
  c{locals = M.insert s val l}

insertLocalM :: (MonadState Context m)=> String->Value-> m()
insertLocalM str val = modify $ insertLocals str val

-- | when applying a function, add the actual parameters to the local context
--   by modifying the existing context. Parameters are passed as a list of name
--   value pairs.
merge :: [(String,Value)] -> Context -> Context
merge params  c@Context {locals=l} =
  c{locals = M.union (M.fromList params) l}

debugM :: (MonadState Context m) => m (M.Map String Value)
debugM = gets locals
