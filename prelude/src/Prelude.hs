{-# LANGUAGE OverloadedStrings #-}
module Prelude ( module Exports
               , voidError
               ) where

import Protolude as Exports hiding (log)
import Protolude.Error (error)

voidError :: Void
voidError = error "Attempted to evaluate a value of type Void"