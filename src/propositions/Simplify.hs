module Simplify where

import Propositions
import SAT

removeRedundancy :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancy axiom (PAnd cs) = simplify $ PAnd $ foldr (\a b -> if (taut $ pimplies (PAnd $ a:b) axiom) then b else a:b) [] $ (removeRedundancy axiom) <$> cs
removeRedundancy axiom x = simplify $ if taut $ pimplies axiom x then PTrue else x