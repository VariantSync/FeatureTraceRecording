module FeatureTrace where

import Tree
import AST
import Data.Maybe
import Propositions
import NullPropositions
import Util

type Feature = String

type FeatureFormula = NullableFormula Feature
data FeatureTrace a = F (Node a -> FeatureFormula)

showTrace :: Show a => FeatureTrace a -> AST a -> Tree String
showTrace (F f) = fmap (\n -> "<"++(show $ f n)++"> "++(show n))

newTrace :: UUID -> Int -> FeatureFormula -> FeatureTrace a
newTrace id version' formula = F(\n -> if (uuid n == id) && (ntype n /= Plain) then formula else Nothing)

{-
Combine two feature traces in the same notion as for functions:
  t1.t2 means t1 'after' t2, i.e. if t2 is undefined on a node, t1 will be used.
  t1 will not overwrite the traces that are already defined by t2.
-}
combine :: FeatureTrace a -> FeatureTrace a -> FeatureTrace a
combine (F t') (F t) = F (\n -> case t n of
    Nothing -> t' n
    Just x -> Just x
    )

{-
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
If the given node is not in the tree, the feature trace of the node will be returned.
-}
pc :: Eq a => AST a -> FeatureTrace a -> Node a -> FeatureFormula
pc root (F trace) node =
  ffand $
  [trace node] ++
  (fmap trace $ fmap element $ legatorAncestors root $ safecrack (tree root node) (\() -> Tree node []))