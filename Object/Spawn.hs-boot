module Object.Spawn where

import Object
import qualified Continuation as C
import {-# SOURCE #-} Eval

spawn :: Object -> EvalM Object
spawnObject :: Object->IO Object
responderObject :: C.Responder Object Message Response
