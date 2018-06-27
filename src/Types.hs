{-# LANGUAGE TemplateHaskell #-}
module Types where

import CustomPrelude

data Image a = Image
  { _width  :: {-# UNPACK #-} !Int
  , _height :: {-# UNPACK #-} !Int
  , _contents :: ()
  }

makeLenses ''Image
