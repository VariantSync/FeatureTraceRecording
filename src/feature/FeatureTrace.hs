module FeatureTrace where

import Feature
import Tree
import AST
import Grammar
import Logic
import Propositions
import NullPropositions
import Data.Set
import Simplify ( removeRedundancy )
import Util ( nothingIf )

type FeatureTrace g a = Node g a -> FeatureFormula

emptyTrace :: FeatureTrace g a
emptyTrace _ = Nothing

simplifyFeatureTrace :: (Grammar g, Show a, Eq a) => FeatureTrace g a -> AST g a -> FeatureTrace g a
simplifyFeatureTrace f t v = case (pc_parentpart t f v, f v) of
  (Just p, Just f') -> nothingIf (==PTrue) (removeRedundancy p f')
  _ -> Propositions.simplify <$> f v

simplifyFeatureTraceOfNodes :: (Grammar g, Show a, Eq a) => FeatureTrace g a -> AST g a -> Set (Node g a) -> FeatureTrace g a
simplifyFeatureTraceOfNodes f t d = \v -> if member v d then simplifyFeatureTrace f t v else f v

{-
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
-}
pc :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc root trace node = land [trace node, pc_parentpart root trace node]

{-
Calculates the parental part of the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
Crashes when the given node is not in the given tree.
(This should be more helpful for debugging instead of just returning Nothing.)
-}
pc_parentpart :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc_parentpart root trace v
  | optionaltype v == Mandatory = parent root t >>= pc root trace . element
  | otherwise = land $ trace.element <$> (optionalAncestors root t)
  where t = tree root v

augmentWithTrace :: (Node g a -> FeatureFormula) -> AST g a -> Tree (FeatureFormula, Node g a)
augmentWithTrace f = fmap (\n -> (f n, n))

prettyPrint :: (Grammar g, Show a) => Tree (FeatureFormula, Node g a) -> String
prettyPrint = Tree.prettyPrint 0 id (\(trace, node) -> "<"++(NullPropositions.prettyPrint trace)++">"++(show node))
