{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Display.Shaders ( CompileError(..)
                       , ShaderSource(..)
                       , VertexShader, pattern VertexShader
                       , TesselationControlShader, pattern TesselationControlShader
                       , TesselationEvaluationShader, pattern TesselationEvaluationShader
                       , GeometryShader, pattern GeometryShader
                       , FragmentShader, pattern FragmentShader
                       , ShaderStage(..)
                       , compile
                       ) where

import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Text.Strict.Lens (utf8)
import Control.Lens.Operators ((#), (??))
import qualified Data.ByteString.Char8 as B
import qualified Foreign as F
import qualified Graphics.GL.Core45 as GL
import qualified Graphics.GL.Types as GL

import Display.ShaderStages

import Data.Singletons

newtype CompileError = CompileError T.Text
newtype ShaderSource = ShaderSource T.Text

data Shader' (i :: ShaderStage) = Shader' { _handle   :: !GL.GLuint
                                          , _uniforms :: ()
                                          }

fromShaderStage :: ShaderStage -> GL.GLenum
fromShaderStage Vertex                = GL.GL_VERTEX_SHADER
fromShaderStage TesselationControl    = GL.GL_TESS_CONTROL_SHADER
fromShaderStage TesselationEvaluation = GL.GL_TESS_EVALUATION_SHADER
fromShaderStage Geometry              = GL.GL_GEOMETRY_SHADER
fromShaderStage Fragment              = GL.GL_FRAGMENT_SHADER

shaderKindEnum :: forall (k :: ShaderStage). SingI k => GL.GLenum
shaderKindEnum = (fromShaderStage . fromSing) (sing @ShaderStage @k)

type VertexShader = Shader' 'Vertex
pattern VertexShader :: GL.GLuint -> VertexShader
pattern VertexShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE VertexShader #-}

type TesselationControlShader = Shader' 'Vertex
pattern TesselationControlShader :: GL.GLuint -> TesselationControlShader
pattern TesselationControlShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE TesselationControlShader #-}

type TesselationEvaluationShader = Shader' 'TesselationEvaluation
pattern TesselationEvaluationShader :: GL.GLuint -> TesselationEvaluationShader
pattern TesselationEvaluationShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE TesselationEvaluationShader #-}

type GeometryShader = Shader' 'Geometry
pattern GeometryShader :: GL.GLuint -> GeometryShader
pattern GeometryShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE GeometryShader #-}

type FragmentShader = Shader' 'Fragment
pattern FragmentShader :: GL.GLuint -> FragmentShader
pattern FragmentShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE FragmentShader #-}

genericCompile :: GL.GLenum -> T.Text -> IO (Either CompileError GL.GLuint)
genericCompile t src = do
  shaderId <- B.useAsCString (utf8 # src) $ \src ->
    F.withArray [src] $ \srcs ->
      GL.glCreateShaderProgramv t 1 srcs
  validate shaderId
  where
    validate :: GL.GLuint -> IO (Either CompileError GL.GLuint)
    validate 0 = pure . Left $ CompileError "Could not allocate a shader object from OpenGL"
    validate x = do
      success <- F.alloca $ \successP -> do
        GL.glGetProgramiv x GL.GL_LINK_STATUS successP
        F.peek successP
      logLength <- F.alloca $ \lenP -> do
        GL.glGetProgramiv x GL.GL_INFO_LOG_LENGTH lenP
        F.peek lenP
      logText <- F.allocaBytes (fromIntegral logLength) $ \logP ->
        F.alloca $ \resultP -> do
          GL.glGetProgramInfoLog x logLength resultP logP
          result <- fromIntegral <$> F.peek resultP
          T.peekCStringLen (logP, result)
      if success == GL.GL_TRUE
        then pure . Right $ x
        else GL.glDeleteProgram x >> (pure . Left $ CompileError logText)

compile :: forall k. SingI k => ShaderSource -> IO (Either CompileError (Shader' k))
compile (ShaderSource src) = fmap (Shader' ?? ()) <$>
  genericCompile (shaderKindEnum @k) src
