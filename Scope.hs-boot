{-#  LANGUAGE FlexibleContexts, KindSignatures  #-}

module Scope where

import  Control.Monad.Except

data VariableContext
data Value (m :: * -> *)

class Scope (m :: * -> *)
