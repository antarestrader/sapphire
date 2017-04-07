-- Object/Runtime.hs Copyright 2017 John F. Miller
{-# OPTIONS_GHC -fno-warn-missing-methods#-}
{-# LANGUAGE OverloadedStrings,  NamedFieldPuns, TypeSynonymInstances,
    MultiParamTypeClasses, RankNTypes, LiberalTypeSynonyms,
    FlexibleInstances #-}
module Object.Runtime where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map.Strict


import Object
import Scope
import Runtime hiding (Runtime)
import Name

instance Scope Runtime where
  readVar IVar name = do
    map <- ivars . objectState <$> getState
    return 
      $   (\obj -> MV {obj,replace=(setVar IVar name)})
      <$> lookup name map
