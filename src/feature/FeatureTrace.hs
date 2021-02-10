module FeatureTrace where

import Feature
import Logic
import Propositions
import NullPropositions
import StructuralElement
import Data.Set
import Simplify ( removeRedundancy )
import Util ( nothingIf )
import Control.Monad (join)

-- Imported these only for printing
import Tree
import Grammar
import AST

type FeatureTrace s = s -> FeatureFormula
type ASTFeatureTrace g a = FeatureTrace (AST g a)

emptyTrace :: FeatureTrace s
emptyTrace _ = Nothing

simplifyFeatureTrace :: (StructuralElement s) => FeatureTrace s -> s -> FeatureTrace s
simplifyFeatureTrace f t v = case (pc_parentpart t f v, f v) of
  (Just p, Just f') -> nothingIf (==PTrue) (removeRedundancy p f')
  _ -> Propositions.simplify <$> f v

simplifyFeatureTraceOfNodes :: (StructuralElement s) => FeatureTrace s -> s -> Set s -> FeatureTrace s
simplifyFeatureTraceOfNodes f t d = \v -> if member v d then simplifyFeatureTrace f t v else f v

{-
Calculates the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
-}
pc :: (StructuralElement s) => s -> FeatureTrace s -> s -> FeatureFormula
pc root trace node = land [trace node, pc_parentpart root trace node]

{-
Calculates the parental part of the presence condition of a node (third argument) in the given tree (first argument) with the given feature traces (second argument).
Crashes when the given node is not in the given tree.
(This should be more helpful for debugging instead of just returning Nothing.)
-}
pc_parentpart :: (StructuralElement s) => s -> FeatureTrace s -> s -> FeatureFormula
pc_parentpart root trace x = join $ se_propagate root trace land x

augmentWithTrace :: (Grammar g, Show a) => ASTFeatureTrace g a -> AST g a -> Tree (FeatureFormula, Node g a)
augmentWithTrace f root = fmap (\n -> (f $ Tree.tree root n, n)) root

prettyPrint :: (Grammar g, Show a) => Tree (FeatureFormula, Node g a) -> String
prettyPrint = Tree.prettyPrint 0 id (\(trace, node) -> "<"++(NullPropositions.prettyPrint trace)++">"++(show node))
