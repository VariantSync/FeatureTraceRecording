
{- |
Description: Data types and functions for feature traces and presence conditions.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Data types and functions for feature traces and presence conditions.
-}
module Feature.FeatureTrace where

import Feature.Feature
import Tree.Tree as Tree
import Tree.AST
import Tree.Grammar
import Propositions.Logic
import Propositions.Propositions as Propositions
import Propositions.NullPropositions as NullPropositions
import Data.Set
import Propositions.Simplify ( removeRedundancy )
import Util ( nothingIf )

-- | A feature trace (or feature mapping) assigns a nullable propositional formula over features to each node in an 'AST'.
-- (See Definition 3.1 in the paper).
type FeatureTrace g a = Node g a -> FeatureFormula

-- | A feature trace with no information.
-- It assigns /null/ ('Nothing') to each node.
-- This feature trace serves as a possible starting point for feature trace recording (e.g., when no domain knowledge was documented in a software project).
emptyTrace :: FeatureTrace g a
emptyTrace _ = Nothing

-- | Simplifies a feature trace over a given 'AST'.
-- A feature formula of a node can be simplified w.r.t. to the feature mappings of all its ancestors.
-- For example, when the root of a tree is mapped to /A/, no descendant has to be mapped to /A/ anymore as they inherit /A/ in their presence condition ('pc').
simplifyFeatureTrace :: (Grammar g, Show a, Eq a) => FeatureTrace g a -> AST g a -> FeatureTrace g a
simplifyFeatureTrace f t v = case (pc_parentpart t f v, f v) of
  (Just p, Just f') -> nothingIf (==PTrue) (removeRedundancy p f')
  _ -> Propositions.simplify <$> f v

-- | Simplifies the feature mappings for all nodes in the given set from the given AST.
-- The given set is assumed to be a subset of the nodes in the tree.
-- For further information, see 'simplifyFeatureTrace'.
simplifyFeatureTraceOfNodes :: (Grammar g, Show a, Eq a) => FeatureTrace g a -> AST g a -> Set (Node g a) -> FeatureTrace g a
simplifyFeatureTraceOfNodes f t d = \v -> if member v d then simplifyFeatureTrace f t v else f v

{- |
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
See Equation 1 in the paper.
-}
pc :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc root trace node = land [trace node, pc_parentpart root trace node]

{- |
Calculates the parental part of the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
Crashes when the given node is not in the given tree.
(This should be more helpful for debugging instead of just returning Nothing.)
-}
pc_parentpart :: (Grammar g, Show a, Eq a) => AST g a -> FeatureTrace g a -> Node g a -> FeatureFormula
pc_parentpart root trace v
  | optionaltype v == Mandatory = parent root t >>= pc root trace . element
  | otherwise = land $ trace.element <$> (optionalAncestors root t)
  where t = tree root v

-- | Pretty print the 'AST' and annotate all nodes with their feature mappings.
prettyPrint :: (Grammar g, Show a) => FeatureTrace g a -> AST g a -> String
prettyPrint trace = Tree.prettyPrint 0 id (\node -> "<"++(NullPropositions.prettyPrint $ trace node)++">"++(show node))
