module DevelMain where
import Graphics.Gloss

glossTest :: IO ()
glossTest = display (InWindow "[gloss]: floating" (800, 600) (0,0))
                    white
                    picture
  where picture =   Translate 0 0
                  $ Scale 0.5 0.5
                  $ Text "Hello World!"
