module FeatureTrace where

import UUID
import Tree
import AST
import Data.Maybe
import Propositions
import NullPropositions
import Util

type Feature = String

type NonNullFeatureFormula = PropositionalFormula Feature
type FeatureFormula = NullableFormula Feature
type FeatureTrace a = Node a -> FeatureFormula

emptyTrace :: FeatureTrace a
emptyTrace = \_ -> Nothing

newTrace :: UUID -> Int -> FeatureFormula -> FeatureTrace a
newTrace id version' formula = \n -> if (uuid n == id) && (ntype n /= Plain) then formula else Nothing

{-
Combine two feature traces in the same notion as for functions:
  t1.t2 means t1 'after' t2, i.e. if t2 is undefined on a node, t1 will be used.
  t1 will not overwrite the traces that are already defined by t2.
-}
combine :: FeatureTrace a -> FeatureTrace a -> FeatureTrace a
combine t1 t2 = \n -> case t2 n of
    Nothing -> t1 n
    Just x -> Just x

{-
Calculates the parental part of the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
Crashes when the given node is not in the given tree.
(This should be more helpful for debugging instead of just returning Nothing.)
-}
pc_parentpart :: (Show a, Eq a) => AST a -> FeatureTrace a -> Node a -> FeatureFormula
pc_parentpart root trace node
  | ntype node == Plain = (parent root $ tree root node) >>= \p -> pc root trace $ element p
  | otherwise = nullable_and $ fmap trace $ fmap element $ legatorAncestors root $ tree root node --(\() -> Tree node [])

{-
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
-}
pc :: (Show a, Eq a) => AST a -> FeatureTrace a -> Node a -> FeatureFormula
pc root trace node = nullable_and [trace node, pc_parentpart root trace node]

augmentWithTrace :: (Node a -> FeatureFormula) -> AST a -> Tree (FeatureFormula, Node a)
augmentWithTrace f = fmap (\n -> (f n, n))

prettyPrint :: (Show a) => Tree (FeatureFormula, Node a) -> String
prettyPrint = Tree.prettyPrint 0 id (\(trace, node) -> "<"++(NullPropositions.prettyPrint trace)++">"++(show node))
