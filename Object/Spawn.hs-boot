module Object.Spawn where

import Object
import qualified Continuation as C

spawn :: Value -> IO Object

responderPrim :: C.Responder Value Message Value
responderObject :: C.Responder Object Message Value
