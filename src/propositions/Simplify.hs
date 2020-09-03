module Simplify (
    removeRedundancy
) where

import Propositions
import SAT

removeRedundancy :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancy axiom (PAnd cs) =
    if contradicts $ PAnd $ axiom:cs
    then PFalse
    else simplify
        $ PAnd
        $ foldr (\elementToInspect b -> if taut $ pimplies (PAnd $ elementToInspect:b) axiom then b else elementToInspect:b) []
        $ (removeRedundancy axiom) <$> cs
removeRedundancy axiom (POr cs) =
      removeRedundancyBase axiom
    $ simplify
    $ POr
    $ foldr (\elementToInspect b -> if contradicts $ PAnd [elementToInspect, axiom] then b else elementToInspect:b) []
    $ (removeRedundancy axiom) <$> cs
removeRedundancy axiom x =
    let y = removeRedundancyBase axiom x in
    if contradicts $ PAnd [y, axiom] then PFalse else y

removeRedundancyBase :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancyBase axiom x = simplify $ if taut $ pimplies axiom x then PTrue else x