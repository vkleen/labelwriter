{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# language NoMonomorphismRestriction #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
module DevelMain where

import Display
import Types
import Data.Array.Repa (Z(..), (:.)(..), D, DIM3, DIM2)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.Vector as RV
import qualified Data.Array.Repa.Algorithms.Randomish as Repa
import qualified Data.Vector.Unboxed as VU

import qualified Codec.Picture.Types as JP (Image(..), PixelRGBA8)

import Data.Colour.Names
import Data.Colour.CIE
import Data.Colour.SRGB.Linear
import Diagrams hiding (render, D)
import Diagrams.Backend.Rasterific

testData :: Int -> Image Repa.D
testData = Repa.map (== 1) . Repa.randomishIntArray (Z :. 672 :. 672) 0 1

displayTest :: IO ()
displayTest = showImage' . render $ testDiagram

testDiagram :: Diagram B
testDiagram = (arrowBetween (p2 (-0.5,-0.5)) (p2 (0.5,0.5)) # lc red <> square 1 # fc yellow)
  # lc cyan
  # bgFrame 0.1 lightgrey

juicyToImage :: JP.Image JP.PixelRGBA8 -> Repa.Array D DIM2 Float
juicyToImage (JP.Image w h dat) =
    (\img -> Repa.traverse img (\(Z :. y :. x :. _) -> Z :. y :. x) greyscale)
  . RV.fromVector (Z :. h :. w :. 4)
  . VU.convert
  $ dat

greyscale :: (DIM3 -> Word8) -> DIM2 -> Float
greyscale f (Z :. y :. x) = luminance c
  where r = scale * fromIntegral (f (Z :. y :. x :. 0))
        g = scale * fromIntegral (f (Z :. y :. x :. 1))
        b = scale * fromIntegral (f (Z :. y :. x :. 2))
        c = rgb r g b
        scale = 1.0 / fromIntegral (maxBound::Word8)

render :: Diagram B -> Repa.Array D DIM2 Float
render = juicyToImage . renderDia Rasterific (RasterificOptions (mkWidth 672))