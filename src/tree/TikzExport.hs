{- |
Facilities for exporting 'AST's to Tikz code as a String.
We use this export to generate the 'AST' figure in the paper (Figure 5).
-}
module TikzExport where

import Tree (element,  Tree(Tree) )
import AST
import Grammar
import Propositions
import Feature
import FeatureTrace
import FeatureTraceRecording
import Util
import Data.List ( intercalate )

{-This file is a bit hacky.-}

-- | Default implementation to export an AST with feature traces.
-- The returned String is the tikz code that can be copied to a tex document.
astToTikzWithTraceDefault :: (Eq a, Show a, Grammar g) => Version g a -> String
astToTikzWithTraceDefault =
    let postProcessing = drop 6 in -- TODO: Fix the hacky "drop 6" that only works for SJava_Grammar
    astToTikzWithTrace
    (tikzifyName
        .(\n ->
            let ruleStr = postProcessing.removeQuotes.show.grammartype $ n 
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

astToTikzWithTrace :: (AST g a -> String) -> (AST g a -> Node g a -> FeatureTrace g a -> String) -> Version g a -> String
astToTikzWithTrace val toCls (trace, t) = astToTikz val (\s -> toCls t (element s) trace) t

astToTikz :: (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikz val cls t = "\\"++(astToTikzRecursive 0 val cls t)++";"

astToTikzRecursive :: Int -> (AST g a -> String) -> (AST g a -> String) -> AST g a -> String
astToTikzRecursive i val cls t@(Tree n cs) = intercalate " " $
    ["node", "[", cls t, "] {", val t, "}"]++
    (mconcat $ fmap (\c -> ["\n", ind, "child", "{", astToTikzRecursive (i+1) val cls c, "\n", ind, "}"]) cs)
    where ind = genIndent $ 2*i

-- | Converts a string to a valid string inside tikz (e.g., escaping certaing characters such as @_@).
tikzifyName :: String -> String
tikzifyName s =
    let replace '_' = "\\_"
        replace '-' = "-{}"
        replace x = [x]
        in
    mconcat $ replace <$> s