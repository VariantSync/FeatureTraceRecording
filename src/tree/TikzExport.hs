module TikzExport where

import Tree (element,  Tree(Tree) )
import AST (Node, value,  AST )
import Propositions
import FeatureTrace
import Util
import Data.List ( intercalate )

astToTikzWithTraceDefault :: (Show a) => FeatureTrace g a -> AST g a -> String
astToTikzWithTraceDefault = astToTikzWithTrace (removeQuotes.show.value.element) featuresToTikzClass

featuresToTikzClass :: FeatureFormula -> String
featuresToTikzClass Nothing = "null"
featuresToTikzClass (Just PTrue) = "true"
featuresToTikzClass (Just PFalse) = "false"
featuresToTikzClass (Just (PVariable v)) = v
featuresToTikzClass _ = error "Only literals are supported"

astToTikzWithTrace :: (AST g a -> String) -> (FeatureFormula -> String) -> FeatureTrace g a -> AST g a -> String
astToTikzWithTrace val toCls trace = astToTikz val (toCls.trace.element)

astToTikz :: (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikz val cls t = "\\"++(astToTikzRecursive 0 val cls t)

astToTikzRecursive :: Int -> (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikzRecursive i val cls t@(Tree n cs) = intercalate " " $
    ["node", "[", cls t, "] {", val t, "}"]++
    (mconcat $ fmap (\c -> ["\n", ind, "child", "{", astToTikzRecursive (i+1) val cls c, "\n", ind, "}"]) cs)
    where ind = genIndent $ 2*i