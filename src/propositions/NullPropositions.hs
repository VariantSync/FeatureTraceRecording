module NullPropositions where

import Propositions

-- The Nothing case represents null
type NullableFormula a = Maybe (PropositionalFormula a)

{-
Combines a list of nullable formulas with the AND operator according where "Nothing && x = x" for any nullable formula x.
-}
ffand :: [NullableFormula a] -> NullableFormula a
ffand [] = Nothing
ffand (x:xs) = case x of
    Nothing -> ffand xs
    Just y -> case ffand xs of
        Nothing -> Just y
        Just z -> Just (PAnd y z)