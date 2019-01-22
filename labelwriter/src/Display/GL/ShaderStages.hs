{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
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

module Display.GL.ShaderStages ( ShaderStage(..)
                               , ValidStages
                               ) where

import Prelude hiding (Set, TypeError, Text, Map)

import Data.Singletons.Prelude
import Data.Singletons.TH
import GHC.TypeLits


singletons [d|
  data ShaderStage = Vertex
                   | TesselationControl
                   | TesselationEvaluation
                   | Geometry
                   | Fragment
    deriving (Show, Eq, Ord, Bounded, Enum)

  data ValidStagesConstraint = HasVertex
                             | TCImpliesTE
    deriving (Eq, Ord, Bounded, Enum)

  allConstraints :: [ValidStagesConstraint]
  allConstraints = [minBound .. maxBound]

  constrain :: [ShaderStage] -> ValidStagesConstraint -> Bool
  constrain xs HasVertex   = Vertex `elem` xs
  constrain xs TCImpliesTE = (TesselationControl `notElem` xs) || (TesselationEvaluation `elem` xs)

  type family ConstraintErrorMessage (k :: ValidStagesConstraint) :: ErrorMessage where
    ConstraintErrorMessage HasVertex   =
      Text "Every pipeline must have a vertex shader."
    ConstraintErrorMessage TCImpliesTE =
      Text "If a pipeline has a tesselation control stage then it must have a tesselation evaluation stage as well."

  checkValidStages :: [ShaderStage] -> Bool
  checkValidStages xs = all (constrain xs) allConstraints

  failingConstraints :: [ShaderStage] -> [ValidStagesConstraint]
  failingConstraints xs = filter (not . constrain xs) allConstraints
  |]

type family ValidStages (stages :: [ShaderStage]) :: Constraint where
  ValidStages stages = If (CheckValidStages stages) ( () :: Constraint )
                          (TypeError ( ShowType stages :<>: Text " is not a valid list of pipeline stages."
                                       :$$: ProduceErrorMessage (FailingConstraints stages)
                                     ))

type family ProduceErrorMessage (ks :: [ValidStagesConstraint]) :: ErrorMessage where
  ProduceErrorMessage ks = Foldr1 (TyCon2 (:$$:)) (Map ConstraintErrorMessageSym0 ks)
