-- Boot.hs Copyright 2013, 2014 John F. Miller

-- | This module which exports a single function, @boot@, is responsible for
--   setting up the runtime system.  Note: if you are looking for the place
--   where all the built-in classes get loaded, it is in Builtin/Bindings.hs

module Boot (boot) where

import Object
import Boot.Options


baseLibrary = "lib/base.sap"

-- | This function builds up the initial runtime. The run time includes
--   Object and Class classes with the internal functions installed. The
--   object returned is an instance of Object sutable to running code.
boot :: Options -> Runtime () -> IO ()
boot = undefined

