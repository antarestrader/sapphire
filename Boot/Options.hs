{-# LANGUAGE CPP #-}
module Boot.Options where

ver = "Sapphire Interperter (Early Developmet Version).  "
    ++ "Copyright 2013-2017 John Miller\n"
    ++ "  Built " ++ __DATE__ ++ " " ++ __TIME__

data Options = Options 
  { debug :: Bool
  , version :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { debug  = False
  , version = ver
  }
