module Propositions where

data PropositionalFormula a =
    PTrue
    | PFalse
    | PVariable a
    | PNot (PropositionalFormula a)
    | PAnd (PropositionalFormula a) (PropositionalFormula a)
    | POr (PropositionalFormula a) (PropositionalFormula a)
    deriving (Show, Eq)

pand :: [PropositionalFormula a] -> PropositionalFormula a
pand [] = PTrue
pand (x:xs) = PAnd x (pand xs)