module Types where

import           Data.LWPixel
import qualified Data.Vector.Unboxed.LWPixel ()
import Data.Array.Repa

type Image r = Array r DIM2 Bool
type PackedImage r = Array r DIM2 LWPixel