{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Data.Pixel
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Pixel ()
import           Control.Lens

data Image = Image
  { _width    :: {-# UNPACK #-} !Int
  , _height   :: {-# UNPACK #-} !Int
  , _contents :: U.Vector Pixel
  }

makeLenses ''Image
