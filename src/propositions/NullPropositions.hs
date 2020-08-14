module NullPropositions where

import Propositions

-- The Nothing case represents null
type NullableFormula a = Maybe (PropositionalFormula a)

{-
Combines a list of nullable formulas with the AND operator according where "Nothing && x = x" for any nullable formula x.
-}
ffand :: [NullableFormula a] -> NullableFormula a
ffand [] = Nothing
ffand (Nothing:xs) = ffand xs
ffand ((Just x):xs) = case ffand xs of
    Nothing -> Just x
    Just x' -> Just (PAnd x x')