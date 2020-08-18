module Propositions where

import Data.List

data PropositionalFormula a =
    PTrue
    | PFalse
    | PVariable a
    | PNot (PropositionalFormula a)
    | PAnd [PropositionalFormula a]
    | POr [PropositionalFormula a]
    deriving (Show, Eq)

data Configuration a = Configuration (a -> Bool)

-- type CNF a = CNF (PAnd [POr a])

pimplies :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
pimplies a b = POr [(PNot a), b]

pand :: [PropositionalFormula a] -> PropositionalFormula a
pand [] = PTrue
pand l = PAnd l

pnegate :: PropositionalFormula a -> PropositionalFormula a
pnegate PTrue = PFalse
pnegate PFalse = PTrue
pnegate p = PNot p

eval :: Configuration a -> PropositionalFormula a -> Bool
eval _ PTrue = True
eval _ PFalse = False
eval (Configuration c) v@(PVariable x) = c x
eval config (PNot x) = not $ eval config x
eval config (PAnd cs) = and $ fmap (eval config) cs
eval config (POr cs) = or $ fmap (eval config) cs

isLiteral :: PropositionalFormula a -> Bool
isLiteral PTrue = True
isLiteral PFalse = True
isLiteral (PVariable x) = True
isLiteral (PNot f) = isLiteral f
isLiteral _ = False

isCNF :: PropositionalFormula a -> Bool
isCNF (PAnd cs) = all isCNF cs
isCNF (POr cs) = all isLiteral cs
isCNF p = isLiteral p

toCNF :: PropositionalFormula a -> PropositionalFormula a
toCNF PTrue = PTrue
toCNF PFalse = PFalse
toCNF v@(PVariable x) = v
toCNF n@(PNot a) = case a of
    PTrue -> PFalse
    PFalse -> PTrue
    (PVariable x) -> n
    (PNot x) -> toCNF x
    (PAnd cs) -> toCNF $ POr $ fmap pnegate cs
    (POr cs) -> toCNF $ PAnd $ fmap pnegate cs
toCNF a@(PAnd cs) = simplify $ PAnd $ fmap toCNF cs
toCNF o@(POr cs) = simplify $ POr $ fmap toCNF cs -- TODO

simplify :: PropositionalFormula a -> PropositionalFormula a
simplify (PNot a) = case simplify a of
    PTrue -> PFalse
    PFalse -> PTrue
    (PNot x) -> x
    p -> PNot p
simplify (PAnd cs) =
    case PAnd (concat $ flip fmap cs (\c -> case simplify c of
        PAnd cs' -> cs'
        p -> [p])) of
            PAnd [] -> PTrue
            p -> p
simplify (POr cs) =
    case POr (concat $ flip fmap cs (\c -> case simplify c of
        POr cs' -> cs'
        p -> [p])) of
            POr [] -> PFalse
            p -> p
simplify p = p