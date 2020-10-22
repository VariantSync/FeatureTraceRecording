module FeatureTrace where

import Tree
import AST
import Propositions
import NullPropositions
import Simplify ( removeRedundancy )
import Util ( nothingIf )

type Feature = String
toFeature :: String -> Feature
toFeature = id

type NonNullFeatureFormula = PropositionalFormula Feature
type FeatureFormula = NullableFormula Feature
type FeatureTrace g a = Node g a -> FeatureFormula

emptyTrace :: FeatureTrace g a
emptyTrace = \_ -> Nothing

simplify :: (Grammar g, Show a, Eq a) => FeatureTrace g a -> AST g a -> FeatureTrace g a
simplify f t v = case (pc_parentpart t f v, f v) of
  (Just p, Just f') -> nothingIf (==PTrue) (removeRedundancy p f')
  _ -> Propositions.simplify <$> f v

{-
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
-}
pc :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc root trace node = nullable_and [trace node, pc_parentpart root trace node]

{-
Calculates the parental part of the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
Crashes when the given node is not in the given tree.
(This should be more helpful for debugging instead of just returning Nothing.)
-}
pc_parentpart :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc_parentpart root trace v
  | ntype v == Mandatory = parent root t >>= \p -> pc root trace $ element p
  | otherwise = nullable_and $ trace.element <$> (treeoptionalAncestors root t)
  where t = tree root v

augmentWithTrace :: (Node g a -> FeatureFormula) -> AST g a -> Tree (FeatureFormula, Node g a)
augmentWithTrace f = fmap (\n -> (f n, n))

prettyPrint :: (Grammar g, Show a) => Tree (FeatureFormula, Node g a) -> String
prettyPrint = Tree.prettyPrint 0 id (\(trace, node) -> "<"++(NullPropositions.prettyPrint trace)++">"++(show node))
