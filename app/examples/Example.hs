module Example where

import AST ( AST )
import Edits ( EditScript )
import FeatureColour ( FeatureColourPalette )
import FeatureTrace ( FeatureTrace, FeatureFormula ) 
import System.Terminal ( MonadColorPrinter )

data Example m g a = Example {
    name :: String,
    startTrace :: FeatureTrace g a,
    startTree :: AST g a,
    editscript :: EditScript g a,
    featurecontexts :: [FeatureFormula],
    colours :: FeatureColourPalette m
}
