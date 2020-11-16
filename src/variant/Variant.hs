module Variant where

import Configuration
import FeatureTrace
import FeatureTraceRecording
import AST
import Edits

data Variant g a = Variant {
    config :: Configuration,
    trace :: FeatureTrace g a,
    artefacts :: AST g a,
    history :: RecordedEditScript g a,
    initialTrace :: FeatureTrace g a,
    initialArtefacts :: AST g a
}

instance Eq a => Eq (Variant g a) where
    v == w = (config v) == (config w)

createVariant :: Configuration -> FeatureTrace g a -> AST g a -> Variant g a
createVariant c t a = Variant {
    config = c,
    trace = t,
    artefacts = a,
    history = [],
    initialTrace = t,
    initialArtefacts = a
}

evolve :: RecordedEditScript g a -> Variant g a -> Variant g a
evolve edits variant = variant { trace = newTrace, artefacts = newTree, history = (history variant)++edits}
    where (newTrace, newTree) = featureTraceRecording defaultFeatureTraceRecording (trace variant) (artefacts variant) edits
