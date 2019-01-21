{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}
module Display.Pipeline where

import Display.ShaderStages
import Display.Shaders
import Data.Type.Set
import Data.Singletons
import qualified Graphics.GL.Types as GL

type PipelineAttachments (stages :: [ShaderStage]) =
  forall (k :: ShaderStage). Member k stages => Sing k -> Maybe (Shader' k)

data Pipeline (stages :: [ShaderStage]) =
  ValidStages stages => Pipeline { _attachments :: PipelineAttachments stages
                                 , _uniforms :: ()
                                 }

data ValidPipeline (stages :: [ShaderStage]) =
  ValidStages stages => ValidPipeline { _handle :: GL.GLuint
                                      , _uniforms :: ()
                                      }