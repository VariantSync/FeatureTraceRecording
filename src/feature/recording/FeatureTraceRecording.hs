{- |
Module: FeatureTraceRecording
Description: Data types and interfaces for feature trace recording.
Copyright: (c) Paul Maximilian Bittner, 2021
License: GPL-3
Maintainer: paul.bittner@uni-ulm.de

Data types and interfaces for feature trace recording.
-}
module FeatureTraceRecording where

import Edits
import AST
import Feature
import FeatureTrace

-- | The feature context is a nullable propositional formula over features to tag edits to source code with the feature that was edited.
type FeatureContext = FeatureFormula

{- |
Encapsulates an edit that was made under a given 'FeatureContext'.
This relation might be established upon commit to version control or via IDE interaction (see Section 4.1 in the paper).
-}
type RecordedEdit g a = (Edit g a, FeatureContext)
type History g a = [RecordedEdit g a]

{- |
We denote the state of a software system at any point in time as a 'Version'.
We describe a version by the software artefacts in terms of an 'AST' and the according 'FeatureTrace's we have at that version.
-}
type Version g a = (FeatureTrace g a, AST g a)

{- |
Type of recording functions for individual edit types.
In the paper, these edits are referred to as R_ins, R_del, R_mov, and R_up.
Instead of passing the delta, we give the entire edit as input to the function from which the delta can be calculated.
-}
type RecordingFunction g a = RecordedEdit g a -> Version g a -> FeatureTrace g a

{- |
Abstraction over Algorithm 1 in the paper.
For different use cases and to prevent future errors, feature trace recording is configurable.
In a framework-like manner, it gives us an implementation for a recording function identified by edit typs.
-}
type FeatureTraceRecording g a = EditType -> RecordingFunction g a

{- |
Runs the given 'FeatureTraceRecording' implementation starting at the given 'Version'.
All edits in the given 'History' will be applied to the given version yielding the new version after that history.
-}
runFTR :: (Show a, Eq a) => FeatureTraceRecording g a -> Version g a -> History g a -> Version g a
runFTR ftr startVersion history = last $ runFTRWithIntermediateSteps ftr startVersion history

{- |
Similar to 'runFTR' but also returns all intermediate results.
-}
runFTRWithIntermediateSteps :: (Show a, Eq a) => FeatureTraceRecording g a -> Version g a -> History g a -> [Version g a]
runFTRWithIntermediateSteps ftr startVersion = scanl record startVersion
    where record (f_old, t_old) recordedEdit@(edit, _)
            = (ftr (edittype edit) recordedEdit (f_old, t_old), run edit t_old)
