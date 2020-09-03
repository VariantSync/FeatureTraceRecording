module NullPropositions where

import Propositions
import Util
import Data.List
import Data.Maybe (isNothing, isJust, fromJust, catMaybes)

-- The Nothing case represents null
type NullableFormula a = Maybe (PropositionalFormula a)

isnull :: NullableFormula a -> Bool
isnull = isNothing

notnull :: NullableFormula a -> Bool
notnull = not.isnull

assure :: NullableFormula a -> PropositionalFormula a
assure = fromJust

{-
Combines a list of nullable formulas with the AND operator according where "Nothing && x = x" for any nullable formula x.
-}
nullable_and :: [NullableFormula a] -> NullableFormula a
nullable_and l = case catMaybes l of
    [] -> Nothing
    justs -> Just $ PAnd justs

prettyPrint :: (Show a) => NullableFormula a -> String
prettyPrint Nothing = "null" -- null, nothing, empty, unknown
prettyPrint (Just p) = show p