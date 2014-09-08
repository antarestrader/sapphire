module Object.Spawn where

import Object
import qualified Continuation as C

spawn :: Value -> IO Object

responderObject :: C.Responder Object Message Value
