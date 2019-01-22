{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Display.GL.Pipeline where

import           Data.Coerce
import           Data.Singletons.Prelude.List
import           Display.GL.ShaderStages
import           Display.GL.Shaders
import qualified Graphics.GL.Types as GL

newtype PipelineBuilderT s (stages :: [ShaderStage]) m a =
  PipelineBuilderT { runPipelineBuilderT :: m a }

ipure :: Monad m => a -> PipelineBuilderT s '[] m a
ipure = PipelineBuilderT . pure

ibind :: Monad m => PipelineBuilderT s stages m a -> (a -> PipelineBuilderT s stages' m b)
                 -> PipelineBuilderT s (Sort (Union stages stages')) m b
ibind x f = PipelineBuilderT $ runPipelineBuilderT x >>= (runPipelineBuilderT . f)

ithen :: Monad m => PipelineBuilderT s stages m a -> PipelineBuilderT s stages' m b
                 -> PipelineBuilderT s (Sort (Union stages stages')) m b
ithen x y = ibind x (const y)

data Pipeline =
  Pipeline { _handle :: GL.GLuint , _uniforms :: () }

buildPipeline :: forall (stages :: [ShaderStage]). ValidStages stages
              => (forall (s :: Type). PipelineBuilderT s stages IO ())
              -> IO Pipeline
buildPipeline x = runPipelineBuilderT x >> pure Pipeline { _handle = 0
                                                         , _uniforms = ()
                                                         }

attach ::  forall (k :: ShaderStage) s. ShaderSource -> PipelineBuilderT s '[k] IO ()
attach = PipelineBuilderT . putText . coerce

vertex :: ShaderSource -> PipelineBuilderT s '[ 'Vertex ] IO ()
vertex = attach

tesselationControl :: ShaderSource -> PipelineBuilderT s '[ 'TesselationControl ] IO ()
tesselationControl = attach

tesselationEvaluation :: ShaderSource -> PipelineBuilderT s '[ 'TesselationEvaluation ] IO ()
tesselationEvaluation = attach

geometry :: ShaderSource -> PipelineBuilderT s '[ 'Geometry ] IO ()
geometry = attach

fragment :: ShaderSource -> PipelineBuilderT s '[ 'Fragment ] IO ()
fragment = attach
