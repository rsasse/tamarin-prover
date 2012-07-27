{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns       #-}
-- |
-- Copyright   : (c) 2010-2012 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC only
--
-- This module implements skolemization of temporal variables, i.e.,
-- guaranteeing that they are fully distinct. It is considered experimental.
--
module Theory.Constraint.Solver.Skolemize (

    skolemizeTemporalVariables
  , simplifyDistinctions
  , violatedDistinctions
  ) where

-- import           Debug.Trace
--
-- import           Prelude                            hiding (id, (.))
--
-- import qualified Data.DAG.Simple                    as D
-- import           Data.Data
-- import           Data.Either                        (partitionEithers)
-- import qualified Data.Foldable                      as F
-- import           Data.List
-- import qualified Data.Map                           as M
-- import           Data.Monoid                        (Monoid(..))
import qualified Data.Set                           as S

-- import           Control.Basics
-- import           Control.Category
-- import           Control.Monad.Disj
-- import           Control.Monad.Fresh
-- import           Control.Monad.Reader
-- import           Control.Monad.State                (gets)
--
--
import           Extension.Data.Label
import           Extension.Prelude
--
-- import           Theory.Constraint.Solver.Goals
-- import           Theory.Constraint.Solver.Reduction
-- import           Theory.Constraint.Solver.Types
import           Theory.Constraint.System
-- import           Theory.Model
-- import           Theory.Text.Pretty


-- | Return all violated distinctions.
violatedDistinctions :: System -> [Distinction]
violatedDistinctions = filter (not . unique) . S.toList . get sDistinctions

-- | Remove subsumed distinctions.
simplifyDistinctions :: System -> System
simplifyDistinctions sys = case violatedDistinctions sys of
    -- make sure we do not accidentally remove contradictions
    [] -> sys
    _  -> modify sDistinctions (S.fromList . dropSubsumed . S.toList) sys
  where
    dropSubsumed = map S.toList . go [] . map S.fromList

    go checked [] = checked
    go checked (dist:dists)
      | any (dist `S.isSubsetOf`) checked ||
        any (dist `S.isSubsetOf`) dists      = go checked        dists
      | otherwise                            = go (dist:checked) dists


skolemizeTemporalVariables = error "skolemizeTemporalVariables: implement"
