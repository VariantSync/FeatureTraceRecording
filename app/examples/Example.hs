module Example where

import AST ( AST )
import Edits ( EditScript )
import Grammar
import Feature
import FeatureTrace
import FeatureTraceRecording
import DefaultFeatureTraceRecording
import FeatureColour (FeatureFormulaColourPalette)

data Example m g a = Example {
    name :: String,
    startTrace :: FeatureTrace g a,
    startTree :: AST g a,
    editscript :: History g a,
    colours :: FeatureFormulaColourPalette m
}

runExample :: (Grammar g, Show a, Eq a) => FeatureTraceRecording g a -> Example m g a -> [(FeatureTrace g a, AST g a)]
runExample ftr example = runFTRWithIntermediateSteps ftr
    (startTrace example)
    (startTree example)
    (editscript example)

runExampleWithDefaultFTR :: (Grammar g, Show a, Eq a) => Example m g a -> [(FeatureTrace g a, AST g a)]
runExampleWithDefaultFTR = runExample defaultFeatureTraceRecording