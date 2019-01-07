{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}
module Display.Pipeline where

import Display.ShaderStages
import qualified Graphics.GL.Types as GL

data Pipeline (stages :: [ShaderStage]) =
  ValidStages stages => Pipeline { _handle :: !GL.GLuint
                                 , _uniforms :: ()
                                 }

test :: ValidStages '[ 'Vertex, 'TesselationControl, 'TesselationEvaluation, 'Fragment ] => ()
test = ()
