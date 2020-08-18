module NullPropositions where

import Propositions
import Data.List

-- The Nothing case represents null
type NullableFormula a = Maybe (PropositionalFormula a)

{-
Combines a list of nullable formulas with the AND operator according where "Nothing && x = x" for any nullable formula x.
-}
ffand :: [NullableFormula a] -> NullableFormula a
ffand l = case [x | Just x <- l] of --I inlined catMaybes here because catMaybes could not be found. I dont know why.
    [] -> Nothing
    justs -> Just $ PAnd justs

    -- ffand [] = Nothing
-- ffand (Nothing:xs) = ffand xs
-- ffand ((Just x):xs) = case ffand xs of
--     Nothing -> Just x
--     Just x' -> Just (PAnd [x,++x')