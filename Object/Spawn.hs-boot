module Object.Spawn where

import Object
import qualified Continuation as C

spawn :: Object -> IO Object

responderObject :: C.Responder Object Message Response
