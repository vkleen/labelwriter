{-# language OverloadedStrings #-}
module Main where

import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa as R
import Graphics.Gloss

main :: IO ()
main = display (InWindow "[gloss]: floating" (800, 600) (0,0))
               white
               picture
  where picture =   Translate (0) (0)
                  $ Scale 0.5 0.5
                  $ Circle 10
