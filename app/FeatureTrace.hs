module FeatureTrace where

import Tree
import FTRNode

data PropositionalFormula x =
    True 
    | False
    | Variable x
    | Not (PropositionalFormula x)
    | And (PropositionalFormula x) (PropositionalFormula x)
    | Or (PropositionalFormula x) (PropositionalFormula x)
    deriving (Show, Eq)

type Feature = String

-- The Nothing case represents null
type FeatureFormula = Maybe (PropositionalFormula Feature)
data FeatureTrace a = F (Node a -> FeatureFormula)

newTrace :: UUID -> Int -> FeatureFormula -> FeatureTrace a
newTrace id version' formula = F(\n -> if (uuid n == id) && (version n == version') then formula else Nothing)

{-
Combine two feature traces in the same notion as for functions:
  t1.t2 means t1 'after' t2, i.e. if t2 is undefined on a node, t1 will be used.
  t1 will not overwrite the traces that are already defined by t2.
-}
(.) :: FeatureTrace a -> FeatureTrace a -> FeatureTrace a
(F t') . (F t) = F (\n -> case t n of
    Nothing -> t' n
    Just x -> Just x
    )


pc :: FTRAST a -> FeatureTrace a -> Node a -> FeatureFormula
pc tree (F trace) node = error "not implemented"
-- PresenceCondition f t = And (f t) (
--   case parent t of
--     Nothing -> PropFormula.True
--     Just p -> case ntype p of
--       Legator -> PresenceCondition f p
--       Plain -> PresenceCondition f p
--