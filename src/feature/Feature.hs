module Feature where

import Logic
import Propositions
import NullPropositions

type Feature = String
toFeature :: String -> Feature
toFeature = id

type NonNullFeatureFormula = PropositionalFormula Feature
type FeatureFormula = NullableFormula Feature