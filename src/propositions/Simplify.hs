module Simplify (
    removeRedundancy
) where

import Propositions
    ( pimplies,
      simplify,
      PropositionalFormula(..) )
import SAT ( contradicts, taut )
import Data.List

{-
  This is a naive implementation for presence condition implementation as described in
    A. von Rhein, A. Grebhahn, S. Apel, N. Siegmund, D. Beyer, and T. Berger.
    Presence-Condition Simplification in Highly Configurable Systems.
    In Proceedings of the IEEE/ACM International Conference on Software Engineering (ICSE), pages 178–188.
    IEEE Computer Society, May 2015.
  As our tool is just a proof-of-concept we implemented an own naive algorithm for presence condition simplification.
  Normally, one should adopt one of the algorithms described in the paper above.
-}
removeRedundancy :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancy axiom (PAnd cs) =
    if contradicts $ PAnd $ axiom:cs
    then PFalse
    else
        simplify $
        PAnd $
        foldr (\elementToInspect b -> if taut $ pimplies axiom (PAnd $ elementToInspect:b) then b else elementToInspect:b) [] $
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

removeRedundancyBase :: (Ord a, Show a) => PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
removeRedundancyBase axiom x = simplify $ if taut $ pimplies axiom x then PTrue else x


 {-
 A further simplifcation on PAnd would be the following.
 Say we have "A and (A or B)".
 This can be simplified to "A".
 How to?
 For a PAnd group to become true, every single clause must be true.
 So for each clause we can assume that all the other clauses are true.
 The following did not work
 (\c -> removeRedundancy (PAnd $ axiom:(delete c cs)) c) <$> cs
 because it lead to
             axiom = T
           formula = ("A" and ("A" and "B"))
simplified formula = "B"
 -}
