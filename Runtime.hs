{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables,
    MultiParamTypeClasses, RankNTypes, FlexibleInstances#-}

module Runtime (
      module Runtime.PID
    , Runtime
    , Obj(..), StateClass(..)
    , getState, putState, apply, call, debug, runGC
    , readResponse
    , spawn, self, bootstrap, mkHole, readHole, writeHole
  ) where

import qualified Prelude
import Prelude hiding (lookup, error)
import Control.Monad.State hiding (state, State)
import qualified Control.Monad.State as St
import Control.Monad.Except
import Control.Concurrent.STM
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import Text.Printf

import Runtime.Runtime
import Runtime.Hole
import Runtime.PID
import Runtime.GarbageCollector
import Name

getState    = Runtime $ gets state
putState st = Runtime $ modify (\rst -> rst{state = st})

instance (MonadError obj) (Runtime st obj) where
  catchError f handler = Runtime $ do
    rts <- get
    let old_err = error rts
        handler' err = do
          modify (\rts-> rts{error= old_err})
          unRuntime $ handler err
    err <- liftIO $ atomically $ mkHole
    put rts{error = err}
    res <- catchError (unRuntime f) handler'
    modify (\rts-> rts{error= old_err})
    return res

  throwError = Runtime . throwError

apply :: (Obj obj, StateClass st obj) =>
         obj
      -> Runtime st obj Response 
      -> Runtime st obj obj
apply alt f = Runtime $ do
    rts <- get
    let old_res = response rts
    res <- liftIO $ atomically $ mkHole
    put rts{response =res}
    unRuntime f
    modify (\rts -> rts{response = old_res})
    liftIO $ atomically $ maybeReadHole alt res

readResponse :: obj -> Runtime st obj obj
readResponse alt = Runtime $  do
  res <- gets response
  liftIO $ atomically $ maybeReadHole alt res

call :: (Obj obj, StateClass st obj) =>PID obj -> Name -> [obj] -> Runtime st obj obj
call pid n ps = Runtime $ do
    RTS{shadows,ourself,error} <- get
    (shadowPID,loop) <- shadowRuntime pid
    let pid'     = M.findWithDefault pid (tID pid) shadows
        shadows' = M.insert tid shadowPID shadows
        tid      = tID $ ourself
    (res, rts) <- liftIO $ do
      res <- atomically $ do
        res <- mkHole
        writePID pid' $ Call n ps shadows' res error
        return res
      loop res
    case res of
      Right obj -> put rts >> return obj
      Left  err -> throwError err

spawn :: (Obj obj, StateClass st obj)
      => st
      -> Fn st obj
      -> Runtime st obj (PID obj)
spawn st f = Runtime $ do
  gc <- gets gc
  pid <- liftIO $ spawnPID gc st f
  return pid

self :: Obj obj => Runtime st obj obj
self = Runtime $ gets (toObj . ourself)

bootstrap :: StateClass st obj=>
              st
          ->  Fn st obj
          ->  IO(PID obj)
bootstrap  st fn = do
  gc <- newGC
  pid <- forkPID (\pid -> register gc pid >> runtime gc pid st fn)
  rootSet gc [pid]
  return pid

runGC :: Runtime st obj ()
runGC = Runtime $ do
  RTS{gc} <- get
  liftIO $ garbageCollector gc

debug :: String -> Runtime st obj ()
debug str = Runtime $ liftIO $ putStrLn str

-- =-=-=-= Private =-=-=-= --

spawnPID :: StateClass st obj=> GC obj -> st ->Fn st obj -> IO (PID obj)
spawnPID gc st f = do
  pid <- forkPID (\pid -> runtime gc pid st f >> printf "  - Ended: %s.\n" (show pid))
  register gc pid
  return pid

runtime :: forall st obj. StateClass st obj=>  GC obj -> PID obj -> st -> Fn st obj -> IO ()
runtime gc pid st f = loop st f
  where
    loop :: StateClass st obj=> st -> Fn st obj -> IO ()
    loop st  f = do
      r <- go st f
      case r of
        Nothing -> return ()
        Just (st',f') -> loop st' f'
    go ::  StateClass st obj => st -> Fn st obj -> IO (Maybe (st, Fn st obj) )
    go st funct = do
      m <- atomically $ readPID pid
      case m of
        Quit -> return Nothing
        Call n ps sm res err -> do
          let rts = RTS{
              response = res
            , error = err
            , state = st
            , gc = gc
            , ourself = pid
            , shadows = sm
            , fn = funct
            }
          result <- run rts (unRuntime $ funct n ps)
          case result of
            Right rts' -> do
              return (Just (state rts', fn rts'))
            Left e -> do
              atomically $ writeHole (error rts) e
              return (Just (state rts, fn rts)) -- should this be (st, funct) instead?
        Mark f -> liftIO (markState st >>= f) >> (return $ Just (st, funct))

data SRT obj = Completed obj | Errored obj | Messaged (Message obj)

shadowRuntime :: forall obj st. StateClass st obj =>
                 PID obj
              -> RunTimeM st obj (
                     (PID obj),
                     (Hole obj -> IO (Either obj obj, RunTimeState st obj))
                   )
shadowRuntime downstream = do
    pid <- liftIO $ newPID
    rts <- get
    let old_res   = response rts
        loop :: StateClass st obj => Hole obj -> IO (Either obj obj, RunTimeState st obj)
        loop res  = loop' rts{response = res}
        loop' :: StateClass st obj => RunTimeState st obj -> IO (Either obj obj, RunTimeState st obj)
        loop' rts = do
          srt <- atomically $
                    (Completed <$> readHole (response rts))
                    `orElse` (Errored <$> readHole (error rts))
                    `orElse` (Messaged <$> readPID pid)
          case srt of
            Completed obj -> return (Right obj, rts{response = old_res})
            Errored err   -> return (Left err, rts{response = old_res})
            Messaged (Mark f) -> (fmap (downstream :) (markRTS rts) >>= f) >> loop' rts
            Messaged (Quit)   -> Prelude.error "WTF! who called QUIT on a shadowed PID?!"
            Messaged (Call n ps sm res err) -> do
              result <- run rts{response = res, error = err} (unRuntime $ (fn rts) n ps)
              case result of
                Right st' -> do
                  loop' rts{state = (state st')}
                Left e -> do
                  atomically $ writeHole err e
                  loop' rts
    return (pid, loop)