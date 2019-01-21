{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}

module Display.ShaderStages ( ShaderStage(..)
                            , ValidStages
                            ) where

import Prelude hiding (Set, TypeError, Text)
import Data.Type.Set

import GHC.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.TH

data ShaderStage = Vertex
                 | TesselationControl
                 | TesselationEvaluation
                 | Geometry
                 | Fragment
  deriving (Show, Eq, Ord)

genSingletons [''ShaderStage]
promoteEqInstance ''ShaderStage
promoteOrdInstance ''ShaderStage

type instance Cmp (a :: ShaderStage) (b :: ShaderStage) = Compare a b

type family TCImpliesTE x :: Bool where
  TCImpliesTE x = Not (MemberP 'TesselationControl x) || MemberP 'TesselationEvaluation x

type family ValidStages (stages :: [ShaderStage]) :: Constraint where
  ValidStages stages = ValidStagesImpl stages (MemberP 'Vertex stages) (TCImpliesTE stages)

type family ValidStagesImpl (stages :: [ShaderStage])
                            (hasVertex :: Bool)
                            (tcImpliesTE :: Bool) :: Constraint where
  ValidStagesImpl _ 'True 'True = ()
  ValidStagesImpl stages 'False 'True = TypeError (
    (ShowType stages :<>: Text " is not a valid list of pipeline stages.")
    :$$:
    Text "Every pipeline must have a vertex shader."
    )
  ValidStagesImpl stages 'True 'False = TypeError (
    (ShowType stages :<>: Text " is not a valid list of pipeline stages")
    :$$:
    Text "If a pipeline has a tesselation control stage then it must have a tesselation evaluation stage as well."
    )
  ValidStagesImpl stages 'False 'False = TypeError (
    (ShowType stages :<>: Text " is not a valid list of pipeline stages")
    :$$:
    Text "Every pipeline must have a vertex shader."
    :$$:
    Text "If a pipeline has a tesselation control stage then it must have a tesselation evaluation stage as well."
    )
