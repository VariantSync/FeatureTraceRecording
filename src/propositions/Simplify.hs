{- |
Description: Simplification of 'PropositionalFormula's.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Functions for simplification of 'PropositionalFormula's.
-}
module Simplify (
    removeRedundancy
) where

import Logic
import Propositions
    ( simplify,
      PropositionalFormula(..) )
import SAT ( contradicts, taut )

{- |
This is a naive implementation for presence condition simplification as described in

@
    A. von Rhein, A. Grebhahn, S. Apel, N. Siegmund, D. Beyer, and T. Berger.
    Presence-Condition Simplification in Highly Configurable Systems.
    In Proceedings of the IEEE/ACM International Conference on Software Engineering (ICSE), pages 178–188.
    IEEE Computer Society, May 2015.
@

This function simplifies a propositional formula (second argument) within the context of another propositional formula (first argument).
Assuming, that the first given formula is always satisfied (i.e., it is an axiom) this function simplifies the second formula.
For example, in pseudo-code @removeRedundancy (A) (A and B) == B@ as @A@ is redundant in the second formula if its already satisfied in the first formula.
-}
removeRedundancy :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancy axiom (PAnd cs) =
    if contradicts $ PAnd $ axiom:cs
    then PFalse
    else
        simplify $
        PAnd $
        foldr (\elementToInspect b -> if taut $ limplies axiom (PAnd $ elementToInspect:b) then b else elementToInspect:b) [] $
        -- (\c -> removeRedundancy (PAnd $ axiom:(delete c cs)) c) <$> cs
        removeRedundancy axiom <$> cs
removeRedundancy axiom (POr cs) =
      removeRedundancyBase axiom $
    --   simplify $
      POr $
      foldr (\elementToInspect b -> if contradicts $ PAnd [elementToInspect, axiom] then b else elementToInspect:b) [] $
      (removeRedundancy axiom) <$> cs
removeRedundancy axiom x =
    let y = removeRedundancyBase axiom x in
    if contradicts $ PAnd [y, axiom] then PFalse else y

-- | Base case for simplification with 'removeRedundancy'.
-- If the @axiom@ always implies @x@ (i.e., @axiom => x@ is a tautology), then @x@ can be simplified to /true/.
removeRedundancyBase :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancyBase axiom x = simplify $ if taut $ limplies axiom x then PTrue else x