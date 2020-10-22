module TikzExport where

import Tree (element,  Tree(Tree) )
import AST 
import Propositions
import FeatureTrace
import Util
import Data.List ( intercalate )

{-This file is a bit hacky.-}

astToTikzWithTraceDefault :: (Eq a, Show a, Grammar g) => FeatureTrace g a -> AST g a -> String
astToTikzWithTraceDefault =
    astToTikzWithTrace
    (tikzifyName
        .(\n ->
            let ruleStr = (drop 5).removeQuotes.show.grammartype $ n -- TODO: Fix the hacky "drop 5" that only works for SCXX_Grammar
                valStr  = removeQuotes.show.value $ n in
            ruleStr++(if valStr /= mempty && valStr /= ruleStr then "\\linebreak\\code{"++valStr++"}" else ""))
        .element)
    (\tree node trace -> intercalate ", " [
        tikzifyName.(\s -> s++if optionaltype node == Mandatory then "Inherited" else "").featuresToTikzClass $ pc tree trace node,
        show $ optionaltype node
      ])

featuresToTikzClass :: FeatureFormula -> String
featuresToTikzClass Nothing = "null"
featuresToTikzClass (Just PTrue) = "true"
featuresToTikzClass (Just PFalse) = "false"
featuresToTikzClass (Just (PVariable v)) = v
featuresToTikzClass (Just (PNot p)) = "not"++(featuresToTikzClass $ Just p)
featuresToTikzClass _ = error "Only literals are supported"

astToTikzWithTrace :: (AST g a -> String) -> (AST g a -> Node g a -> FeatureTrace g a -> String) -> FeatureTrace g a -> AST g a -> String
astToTikzWithTrace val toCls trace t = astToTikz val (\s -> toCls t (element s) trace) t

astToTikz :: (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikz val cls t = "\\"++(astToTikzRecursive 0 val cls t)++";"

astToTikzRecursive :: Int -> (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikzRecursive i val cls t@(Tree n cs) = intercalate " " $
    ["node", "[", cls t, "] {", val t, "}"]++
    (mconcat $ fmap (\c -> ["\n", ind, "child", "{", astToTikzRecursive (i+1) val cls c, "\n", ind, "}"]) cs)
    where ind = genIndent $ 2*i

tikzifyName :: String -> String
tikzifyName s =
    let replace '_' = "\\_"
        replace '-' = "-{}"
        replace x = [x]
        in
    mconcat $ replace <$> s