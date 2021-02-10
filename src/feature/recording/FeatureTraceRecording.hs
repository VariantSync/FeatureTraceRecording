module FeatureTraceRecording where

import Edits
import StructuralElement
import Feature
import FeatureTrace

type FeatureContext = FeatureFormula

{-
Encapsulates an edit that was recorded under a given feature context.
This might happen via IDE interaction or diffing techniques.
-}
type RecordedEdit s = (Edit s, FeatureContext)
type History s = [RecordedEdit s]

{-
We denote the state of a software system at any point in time as a Version.
We describe a version by the software artefacts in terms of an AST and the according feature traces we have at that version.
-}
type Version s = (FeatureTrace s, s)

{-
Type for the recording functions for individual edit types.
In the paper, these edits are referred to as R_ins, R_del, R_mov, and R_up.
Instead of passing the delta, we give the entire edit as input to the function from which the delta can be calculated.
-}
type RecordingFunction s = RecordedEdit s -> Version s -> FeatureTrace s

{-
Abstraction over Algorithm 1 in the paper.
For different use cases and to prevent future errors, feature trace recording is configurable.
In a framework-like manner, it gives us an implementation for a recording function identified by edit types.
-}
type FeatureTraceRecording s = EditType -> RecordingFunction s

{-
Runs the given feature trace recording implementation starting at the given version.
All edits in the given history will be applied to the given version yielding the new version after that history.
-}
runFTR :: (StructuralElement s) => FeatureTraceRecording s -> Version s -> History s -> Version s
runFTR ftr startVersion history = last $ runFTRWithIntermediateSteps ftr startVersion history

{-
The same as runFTR but also returns all intermediate results.
-}
runFTRWithIntermediateSteps :: (StructuralElement s) => FeatureTraceRecording s -> Version s -> History s -> [Version s]
runFTRWithIntermediateSteps ftr startVersion = scanl record startVersion
    where record (f_old, t_old) recordedEdit@(edit, _)
            = (ftr (edittype edit) recordedEdit (f_old, t_old), run edit t_old)
