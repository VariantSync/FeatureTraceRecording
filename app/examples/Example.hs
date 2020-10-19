module Example where

import AST ( AST )
import Edits ( EditScript )
import FeatureTrace
import FeatureColour (FeatureFormulaColourPalette)

data Example m g a = Example {
    name :: String,
    startTrace :: FeatureTrace g a,
    startTree :: AST g a,
    editscript :: EditScript g a,
    featurecontexts :: [FeatureFormula],
    colours :: FeatureFormulaColourPalette m
}
