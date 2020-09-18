module FeatureTrace where

import UUID
import Tree
import AST
import Data.Maybe
import Propositions
import NullPropositions
import Simplify
import Util

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
  (Just p, Just f) -> nothingIf (==PTrue) (removeRedundancy p f)
  _ -> Propositions.simplify <$> f v

{-
Combine two feature traces in the same notion as for functions:
  t1.t2 means t1 'after' t2, i.e. if t2 is undefined on a node, t1 will be used.
  t1 will not overwrite the traces that are already defined by t2.
-}
combine :: FeatureTrace g a -> FeatureTrace g a -> FeatureTrace g a
combine t1 t2 = \n -> case t2 n of
    Nothing -> t1 n
    Just x -> Just x

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
  | ntype v == Plain = parent root t >>= \p -> pc root trace $ element p
  | otherwise = nullable_and $ trace.element <$> (legatorAncestors root t)
  where t = tree root v

augmentWithTrace :: (Node g a -> FeatureFormula) -> AST g a -> Tree (FeatureFormula, Node g a)
augmentWithTrace f = fmap (\n -> (f n, n))

prettyPrint :: (Grammar g, Show a) => Tree (FeatureFormula, Node g a) -> String
prettyPrint = Tree.prettyPrint 0 id (\(trace, node) -> "<"++(NullPropositions.prettyPrint trace)++">"++(show node))
