module Example where

import StructuralElement
import AST ( AST )
import Edits ( EditScript )
import Grammar
import Feature
import FeatureTrace
import FeatureTraceRecording
import DefaultFeatureTraceRecording
import FeatureColour (FeatureFormulaColourPalette)

data Example m s = Example {
    name :: String,
    startVersion :: Version s,
    history :: History s,
    colours :: FeatureFormulaColourPalette m
}

type ASTExample m g a = Example m (AST g a)

runExample :: (StructuralElement s) => FeatureTraceRecording s -> Example m s -> [Version s]
runExample ftr example = runFTRWithIntermediateSteps ftr (startVersion example) (history example)

runExampleWithDefaultFTR :: (StructuralElement s) => Example m s -> [Version s]
runExampleWithDefaultFTR = runExample defaultFeatureTraceRecording