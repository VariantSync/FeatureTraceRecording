module Example where

import AST ( AST )
import Edits ( EditScript )
import Feature
import FeatureTrace
import FeatureTraceRecording
import FeatureColour (FeatureFormulaColourPalette)

data Example m g a = Example {
    name :: String,
    startTrace :: FeatureTrace g a,
    startTree :: AST g a,
    editscript :: RecordedEditScript g a,
    colours :: FeatureFormulaColourPalette m
}
