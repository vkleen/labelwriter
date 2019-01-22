{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-partial-type-signatures #-}
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

import Display.GL.Shaders
import Display.GL.ShaderStages
import Display.GL.Pipeline

import qualified Graphics.GL.Core45 as GL
import qualified Graphics.GL.Types as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Foreign as F

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Text.RawString.QQ

import Prelude
import Data.String (fromString)

testPipeline :: PipelineBuilderT s _ IO ()
testPipeline = do fragment (ShaderSource "FragmentTest")
                  tesselationEvaluation (ShaderSource "blub")
                  vertex (ShaderSource "VertexTest")
  where (>>) = ithen
        return = ipure

test :: IO Pipeline
test = buildPipeline testPipeline

vertexShader :: ShaderSource
vertexShader = ShaderSource [r|#version 450 core
layout (location = 0) in vec2 position;

uniform float scale;
uniform vec2 translate;

out vec2 tex_coord;

oid main() {
  gl_Position = vec4(scale * position + translate, 0, 1);
  tex_coord = (1 + vec2(position.x, -position.y))/2.0;
}
|]

test2 :: IO ()
test2 = do
    GLFW.setErrorCallback (Just errorCallback)
    True <- GLFW.init
    traverse_ GLFW.windowHint [ GLFW.WindowHint'ContextVersionMajor 4
                              , GLFW.WindowHint'ContextVersionMinor 5
                              , GLFW.WindowHint'sRGBCapable True
                              , GLFW.WindowHint'OpenGLDebugContext True
                              , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
                              ]
    Just window <- uncurry GLFW.createWindow windowSize "[gloss]: floating; Pixels"
                                             Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.swapInterval 1
    debugFunPtr <- GL.mkGLDEBUGPROC gldebugCallback
    GL.glDebugMessageCallback debugFunPtr F.nullPtr
    GL.glEnable GL.GL_DEBUG_OUTPUT
    GL.glEnable GL.GL_DEBUG_OUTPUT_SYNCHRONOUS
    traverse_ (\s -> GL.glDebugMessageControl GL.GL_DONT_CARE
                                              GL.GL_DONT_CARE
                                              s
                                              0 F.nullPtr
                                              GL.GL_TRUE)
              [ GL.GL_DEBUG_SEVERITY_NOTIFICATION, GL.GL_DEBUG_SEVERITY_LOW
              , GL.GL_DEBUG_SEVERITY_MEDIUM, GL.GL_DEBUG_SEVERITY_HIGH
              ]
    compile vertexShader >>= either (putText . fromString . displayException)
                                    (\(VertexShader h) -> GL.glDeleteShader h)
    GLFW.destroyWindow window
    GLFW.terminate
    F.freeHaskellFunPtr debugFunPtr
  where
    fail = throwString

    windowSize = (1536, 864)

errorCallback :: (Print a, MonadIO m) => p -> a -> m ()
errorCallback _ = putErrLn
gldebugCallback :: GL.GLenum -> GL.GLenum -> GL.GLuint -> GL.GLenum -> GL.GLsizei -> Ptr GL.GLchar -> Ptr () -> IO ()
gldebugCallback _ _ _ _ length message _ = do
  msg <- T.peekCStringLen (F.castPtr message, fromIntegral length)
  putErrLn $ "OpenGL debug callback: " <> msg


-- testData :: Int -> Image Repa.D
-- testData = Repa.map (== 1) . Repa.randomishIntArray (Z :. 672 :. 672) 0 1

-- displayTest :: IO ()
-- displayTest = showImage' . render $ testDiagram

-- testDiagram :: Diagram B
-- testDiagram = (arrowBetween (p2 (-0.5,-0.5)) (p2 (0.5,0.5)) # lc red <> square 1 # fc yellow)
--   # lc cyan
--   # bgFrame 0.1 lightgrey

-- juicyToImage :: JP.Image JP.PixelRGBA8 -> Repa.Array D DIM2 Float
-- juicyToImage (JP.Image w h dat) =
--     (\img -> Repa.traverse img (\(Z :. y :. x :. _) -> Z :. y :. x) greyscale)
--   . RV.fromVector (Z :. h :. w :. 4)
--   . VU.convert
--   $ dat

-- greyscale :: (DIM3 -> Word8) -> DIM2 -> Float
-- greyscale f (Z :. y :. x) = luminance c
--   where r = scale * fromIntegral (f (Z :. y :. x :. 0))
--         g = scale * fromIntegral (f (Z :. y :. x :. 1))
--         b = scale * fromIntegral (f (Z :. y :. x :. 2))
--         c = rgb r g b
--         scale = 1.0 / fromIntegral (maxBound::Word8)

-- render :: Diagram B -> Repa.Array D DIM2 Float
-- render = juicyToImage . renderDia Rasterific (RasterificOptions (mkWidth 672))
