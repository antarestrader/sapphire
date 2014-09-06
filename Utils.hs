module Utils where

import Control.Monad.Error

guardR :: (Error e, MonadError e m)=> String -> Bool -> m ()
guardR _ True = return ()
guardR msg False = throwError $ strMsg msg
