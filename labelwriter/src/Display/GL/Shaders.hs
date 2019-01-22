{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Display.GL.Shaders ( CompileError(..)
                          , ShaderSource(..)
                          , VertexShader, pattern VertexShader
                          , TesselationControlShader, pattern TesselationControlShader
                          , TesselationEvaluationShader, pattern TesselationEvaluationShader
                          , GeometryShader, pattern GeometryShader
                          , FragmentShader, pattern FragmentShader
                          , Shader'(..)
                          , ShaderStage(..)
                          , compile
                          ) where

import           Control.Lens.Operators ((??))
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Foreign as F
import qualified Graphics.GL.Core45 as GL
import qualified Graphics.GL.Types as GL

import           Display.GL.ShaderStages

import           Data.Singletons

newtype CompileError = CompileError T.Text
  deriving Show
instance Exception CompileError where
  displayException (CompileError t) = T.unpack t

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
shaderKindEnum = (fromShaderStage . fromSing) (sing @k)

type VertexShader = Shader' 'Vertex
pattern VertexShader :: GL.GLuint -> VertexShader
pattern VertexShader s = Shader' { _handle = s, _uniforms = () }
{-# COMPLETE VertexShader #-}

type TesselationControlShader = Shader' 'TesselationControl
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
genericCompile t src = try $ bracketOnError
    (GL.glCreateShader t)
    GL.glDeleteShader
    $ \shaderId -> do when (shaderId == 0) . throw $
                        CompileError "Could not allocate a shader object from OpenGL"
                      T.withCStringLen src $ \(str, len) ->
                        F.withArray [str] $ \strs ->
                        F.withArray [fromIntegral len] $ \lens ->
                          GL.glShaderSource shaderId 1 strs lens
                      GL.glCompileShader shaderId
                      validate shaderId >> pure shaderId
  where
    validate :: GL.GLuint -> IO ()
    validate x = do
      success <- F.alloca $ \successP -> do
        GL.glGetShaderiv x GL.GL_COMPILE_STATUS successP
        F.peek successP
      unless (success == GL.GL_TRUE) $
        do logLength <- F.alloca $ \lenP -> do
             GL.glGetShaderiv x GL.GL_INFO_LOG_LENGTH lenP
             F.peek lenP
           logText <- F.allocaBytes (fromIntegral logLength) $ \logP ->
             F.alloca $ \resultP -> do
               GL.glGetShaderInfoLog x logLength resultP logP
               result <- fromIntegral <$> F.peek resultP
               T.peekCStringLen (logP, result)
           throw $ CompileError logText

compile :: forall k. SingI k => ShaderSource -> IO (Either CompileError (Shader' k))
compile (ShaderSource src) = fmap (Shader' ?? ()) <$>
  genericCompile (shaderKindEnum @k) src
