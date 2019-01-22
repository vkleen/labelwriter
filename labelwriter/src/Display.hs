{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language BangPatterns #-}
module Display ( showImage
               , showImage'
               ) where

import Types
import Data.Array.Repa as Repa
import Data.Array.Repa.Eval as Repa
import Data.Array.Repa.Repr.ForeignPtr as Repa

import qualified Control.Lens as L
import Control.Lens.Operators

import Text.RawString.QQ

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.ByteString.Char8 as B
import qualified Foreign as F
import qualified Graphics.GL.Core45 as GL
import qualified Graphics.GL.Types as GL
import qualified Graphics.UI.GLFW as GLFW

data GraphicsResources = GraphicsResources { _vao              :: GL.GLuint
                                           , _program          :: GL.GLuint
                                           , _scaleUniform     :: GL.GLint
                                           , _translateUniform :: GL.GLint
                                           , _screen           :: GL.GLuint
                                           }
L.makeLenses ''GraphicsResources

vertexShader :: Text
vertexShader = [r|#version 450 core
layout (location = 0) in vec2 position;

uniform float scale;
uniform vec2 translate;

out vec2 tex_coord;

void main() {
  gl_Position = vec4(scale * position + translate, 0, 1);
  tex_coord = (1 + vec2(position.x, -position.y))/2.0;
}
|]

fragmentShader :: Text
fragmentShader = [r|#version 450 core
layout (location = 0) out vec4 color;
layout (binding = 0) uniform sampler2D screen;

in vec2 tex_coord;

void main() {
  color = vec4(texture(screen, tex_coord).rrr, 1);
}
|]

compileShader :: GL.GLenum -> Text -> ExceptT Text IO GL.GLuint
compileShader shaderType source = do
  shaderId <- GL.glCreateShader shaderType
  liftIO $ T.withCStringLen source $ \(str,len) ->
    F.withArray [str] $ \strs ->
    F.withArray [fromIntegral len] $ \lens ->
      GL.glShaderSource shaderId 1 strs lens
  GL.glCompileShader shaderId
  success <- liftIO $ F.alloca $ \successP -> do
    GL.glGetShaderiv shaderId GL.GL_COMPILE_STATUS successP
    F.peek successP
  if success == GL.GL_TRUE
    then pure shaderId
    else do
      logLength <- liftIO $ F.alloca $ \lenP -> do
        GL.glGetShaderiv shaderId GL.GL_INFO_LOG_LENGTH lenP
        F.peek lenP
      logText <- liftIO $ F.allocaBytes (fromIntegral logLength) $ \logP ->
        F.alloca $ \resultP -> do
          GL.glGetShaderInfoLog shaderId logLength resultP logP
          result <- fromIntegral <$> F.peek resultP
          T.peekCStringLen (logP, result)
      GL.glDeleteShader shaderId
      let prefix = case shaderType of
            GL.GL_VERTEX_SHADER -> "Vertex"
            GL.GL_FRAGMENT_SHADER -> "Fragment"
            _ -> "Some"
      throwError $ prefix <> " shader error: " <> logText

linkShaders :: GL.GLuint -> GL.GLuint -> ExceptT Text IO GL.GLuint
linkShaders vs fs = do
  program <- GL.glCreateProgram
  traverse_ (GL.glAttachShader program)
            [vs, fs]
  GL.glLinkProgram program
  success <- liftIO . F.alloca $ \successP -> do
    GL.glGetProgramiv program GL.GL_LINK_STATUS successP
    F.peek successP
  if success == GL.GL_TRUE
    then pure program
    else do
      logLength <- liftIO . F.alloca $ \lenP -> do
        GL.glGetProgramiv program GL.GL_INFO_LOG_LENGTH lenP
        F.peek lenP
      logText <- liftIO . F.allocaBytes (fromIntegral logLength) $ \logP ->
        F.alloca $ \resultP -> do
          GL.glGetProgramInfoLog program logLength resultP logP
          result <- fromIntegral <$> F.peek resultP
          T.peekCStringLen (logP, result)
      GL.glDeleteProgram program
      throwError $ "Shader program link error: " <> logText

-- TODO: add a proper datatype for the return value
compileProgram :: ExceptT Text IO (GL.GLuint, GL.GLint, GL.GLint)
compileProgram = do
  vsId <- compileShader GL.GL_VERTEX_SHADER vertexShader
  fsId <- compileShader GL.GL_FRAGMENT_SHADER fragmentShader
    `catchError` (\e -> GL.glDeleteShader vsId >> throwError e)
  program <- linkShaders vsId fsId
    `catchError` (\e -> do GL.glDeleteShader vsId
                           GL.glDeleteShader fsId
                           throwError e)
  scale <- liftIO . B.useAsCString "scale" $ \name ->
    GL.glGetUniformLocation program name
  translate <- liftIO . B.useAsCString "translate" $ \name ->
    GL.glGetUniformLocation program name
  pure (program, scale, translate)

vertices :: [GL.GLfloat]
vertices = [ -1.0, -1.0
           , -1.0,  1.0
           ,  1.0, -1.0
           ,  1.0,  1.0
           ]

loop :: GLFW.Window -> GraphicsResources -> IO ()
loop w r = do GL.glClearColor 0 0 1 1
              GL.glClear GL.GL_COLOR_BUFFER_BIT
              GL.glUseProgram (r^.program)
              GL.glUniform1f (r^.scaleUniform) 0.5
              GL.glUniform2f (r^.translateUniform) 0.5 0.5
              GL.glBindVertexArray (r^.vao)
              GL.glBindTextureUnit 0 (r^.screen)
              GL.glDrawArrays GL.GL_TRIANGLE_STRIP 0 4
              GL.glFinish
              GLFW.swapBuffers w >> GLFW.waitEvents
              b <- GLFW.windowShouldClose w
              unless b (loop w r)

errorCallback :: (Print a, MonadIO m) => p -> a -> m ()
errorCallback _ = putErrLn
gldebugCallback :: GL.GLenum -> GL.GLenum -> GL.GLuint -> GL.GLenum -> GL.GLsizei -> Ptr GL.GLchar -> Ptr () -> IO ()
gldebugCallback _ _ _ _ length message _ = do
  msg <- T.peekCStringLen (F.castPtr message, fromIntegral length)
  putErrLn $ "OpenGL debug callback: " <> msg

showImage' :: (Load r DIM2 Float) => Repa.Array r DIM2 Float -> IO ()
showImage' img = do
  hPutStr stderr ("Computing... " :: T.Text)
  !screenImg <- Repa.computeP img
  putErrText "done"
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

  ms <- runExceptT $ setup screenImg
  (ms & either putErrLn
               (loop window)) `finally`
    (  GLFW.destroyWindow window
    >> GLFW.terminate
    >> F.freeHaskellFunPtr debugFunPtr)

  where _ :. sizeY :. sizeX = Repa.extent img

        -- XXX: those numbers are my preference for a floating window in the middle of a 1920x1080 screen
        windowSize = ( min 1536 (sizeX + 50)
                     , min 864  (sizeY + 50)
                     )

        setup :: Repa.Array F DIM2 GL.GLfloat -> ExceptT Text IO GraphicsResources
        setup abgr = do
          GL.glEnable GL.GL_FRAMEBUFFER_SRGB
          (program, scaleUniform, translateUniform) <- compileProgram
          GL.glUseProgram program

          vao <- liftIO . F.alloca $ \vaoP -> do GL.glGenVertexArrays 1 vaoP
                                                 F.peek vaoP
          GL.glBindVertexArray vao

          vbo <- liftIO . F.alloca $ \vboP -> do GL.glGenBuffers 1 vboP
                                                 F.peek vboP
          GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo

          liftIO . F.withArray vertices $ \v ->
            GL.glBufferData GL.GL_ARRAY_BUFFER
                            (fromIntegral $ F.sizeOf (0.0 :: GL.GLfloat) * length vertices)
                            (F.castPtr v)
                            GL.GL_STATIC_DRAW
          GL.glVertexAttribPointer 0 2 GL.GL_FLOAT GL.GL_FALSE 0 (F.wordPtrToPtr 0)
          GL.glEnableVertexAttribArray 0

          screen <- liftIO . F.alloca $ \p -> do GL.glCreateTextures GL.GL_TEXTURE_2D 1 p
                                                 F.peek p
          GL.glTextureStorage2D screen 1 GL.GL_R32F (fromIntegral sizeX) (fromIntegral sizeY)
          liftIO . F.withForeignPtr (Repa.toForeignPtr abgr) $ \ptr ->
            GL.glTextureSubImage2D screen 0 0 0 (fromIntegral sizeX) (fromIntegral sizeY)
                                   GL.GL_RED GL.GL_FLOAT (F.castPtr ptr)

          GL.glTextureParameteri screen GL.GL_TEXTURE_MIN_FILTER GL.GL_LINEAR
          GL.glTextureParameteri screen GL.GL_TEXTURE_MAG_FILTER GL.GL_LINEAR

          pure $ GraphicsResources vao program scaleUniform translateUniform screen

showImage :: (Source r Bool) => Image r -> IO ()
showImage img = showImage' frame
  where frame = Repa.map makePixel img

        makePixel :: Bool -> Float
        makePixel True = 0
        makePixel False = 1
