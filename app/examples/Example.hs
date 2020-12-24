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
    startVersion :: Version g a,
    history :: History g a,
    colours :: FeatureFormulaColourPalette m
}

runExample :: (Grammar g, Show a, Eq a) => FeatureTraceRecording g a -> Example m g a -> [Version g a]
runExample ftr example = runFTRWithIntermediateSteps ftr (startVersion example) (history example)

runExampleWithDefaultFTR :: (Grammar g, Show a, Eq a) => Example m g a -> [Version g a]
runExampleWithDefaultFTR = runExample defaultFeatureTraceRecording