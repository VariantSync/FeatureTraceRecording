module FeatureTraceRecording where

import Edits
import AST
import Feature
import FeatureTrace
import Grammar

type FeatureContext = FeatureFormula
data FeasibleFeatureContext =
      None String -- string contains metadata on why there is no feasible feature context
    | WeakerEquals FeatureContext
    | StrongerEquals FeatureContext
    | Exactly FeatureContext
    | OneOf [FeatureContext]
    | Any

{-
Encapsulates an edit that was recorded under a given feature context.
This might happen via IDE interaction or diffing techniques.
-}
type RecordedEdit g a = (Edit g a, FeatureContext)
type History g a = [RecordedEdit g a]

{-
We denote the state of a software system at any point in time as a Version.
We describe the a version by the software artefacts in terms of an AST and the according feature traces we have at that version.
-}
type Version g a = (FeatureTrace g a, AST g a)

{-
Type for the recording functions for individual edit types.
In the paper, these edits are referred to as R_ins, R_del, R_mov, and R_up.
Instead of just passing the delta, we give the entire edit as input to the function from which the delta can be calculated.
-}
data RecordingFunction g a = RecordingFunction {
    runRecording :: RecordedEdit g a -> Version g a -> FeatureTrace g a,
    {-
    Tells us a possible feature context such that the given feature trace would be calculated by `run`
    when the given edit when applied to the given version.
    -}
    reverseEngineerFeatureContext :: Edit g a -> Version g a -> FeatureTrace g a -> FeasibleFeatureContext
}

{-
Abstraction over Algorithm 1 in the paper.
For different use cases and to prevent future errors, feature trace recording is configurable.
In a framework-like manner, it gives us an implementation for a recording function.
-}
type FeatureTraceRecording g a = EditType -> RecordingFunction g a

{-
Runs the given feature trace recording implementation starting at the given version.
All edits in the given history will be applied to the given version yielding the new version after that history.
-}
runFTR :: (Show a, Eq a) => FeatureTraceRecording g a -> Version g a -> History g a -> Version g a
runFTR ftr startVersion history = last $ runFTRWithIntermediateSteps ftr startVersion history

{-
The same as runFTR but also returns all intermediate results.
-}
runFTRWithIntermediateSteps :: (Show a, Eq a) => FeatureTraceRecording g a -> Version g a -> History g a -> [Version g a]
runFTRWithIntermediateSteps ftr startVersion = scanl record startVersion
    where record (f_old, t_old) recordedEdit@(edit, _)
            = (runRecording (ftr (edittype edit)) recordedEdit (f_old, t_old), Edits.run edit t_old)
