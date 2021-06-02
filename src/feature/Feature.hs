{-
Module for representing features.
-}
module Feature where

import Propositions
import NullPropositions

-- | Features are represented by a unique name.
type Feature = String

-- | Construct a feature from a name.
toFeature :: String -> Feature
toFeature = id

-- | 'PropositionalFormula' over 'Feature's
type NonNullFeatureFormula = PropositionalFormula Feature

-- | Nullable 'PropositionalFormula' over 'Feature's
type FeatureFormula = NullableFormula Feature