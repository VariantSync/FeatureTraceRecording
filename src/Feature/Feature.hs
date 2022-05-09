{- |
Description: Data types for features and feature formulas.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Data types for features and feature formulas.
-}
module Feature.Feature where

import Propositions.Propositions
import Propositions.NullPropositions

-- | Features are represented by a unique name.
type Feature = String

-- | Construct a feature from a name.
toFeature :: String -> Feature
toFeature = id

-- | 'PropositionalFormula' over 'Feature's
type NonNullFeatureFormula = PropositionalFormula Feature

-- | Nullable 'PropositionalFormula' over 'Feature's
type FeatureFormula = NullableFormula Feature