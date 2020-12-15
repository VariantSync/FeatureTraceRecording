module FeatureTraceRecording where

import Edits
import AST
import Feature
import FeatureTrace

import Data.Bifunctor ( Bifunctor(first) )

type FeatureContext = FeatureFormula

type RecordedEdit g a = (Edit g a, FeatureContext)
type History g a = [RecordedEdit g a]

type RecordingFunction g a = RecordedEdit g a -> AST g a -> FeatureTrace g a -> FeatureTrace g a
type FeatureTraceRecording g a = EditType -> RecordingFunction g a

-- Inverse of RecordingFunction w.r.t. the feature context.
type ReverseEngineeringFTR g a = Edit g a -> AST g a -> FeatureTrace g a -> FeatureTrace g a -> FeatureContext

{-
Runs the given feature trace recording.
Arguments are:
1. chosen feature trace recording algorithm (for example defaultFeatureTraceRecording)
2. initial feature trace (for example FeatureTrace.emptyTrace)
3. initial source code as AST
4. edit script (i.e., the sequence of edits applied to the initial AST upon which feature traces should be recorded) with the corresponding feature contexts
-}
runFTR :: (Show a, Eq a) => FeatureTraceRecording g a -> FeatureTrace g a -> AST g a -> History g a -> (FeatureTrace g a, AST g a)
runFTR ftr f0 t0 reditscript = last $ runFTRWithIntermediateSteps ftr f0 t0 reditscript

{-
The same as featureTraceRecording but also returns all intermediate results.
-}
runFTRWithIntermediateSteps :: (Show a, Eq a) => FeatureTraceRecording g a -> FeatureTrace g a -> AST g a -> History g a -> [(FeatureTrace g a, AST g a)]
runFTRWithIntermediateSteps ftr f0 t0 = scanl record (f0, t0)
    where record (f_old, t_old) recordedEdit@(edit, _) = ((ftr $ edittype edit) recordedEdit t_old f_old, run edit t_old)

runBackwards :: (Show a, Eq a) => FeatureTraceRecording g a -> FeatureTrace g a -> AST g a -> History g a -> [(FeatureTrace g a, AST g a)]
runBackwards ftr fLast tLast history = reverse $ runFTRWithIntermediateSteps ftr fLast tLast (invertHistory history)

-- reverseEngineerContext :: FeatureTraceRecording g a -> FeatureTrace g a -> AST g a -> FeatureTrace g a -> Edit g a -> FeatureContext
-- reverseEngineerContext ftr f_old t_old f_new edit = 

{-
Given a history
a -> b -> c -> ... -> n
we first invert all the edits and get
a <- b <- c <- ... <- n
and then reverse the entire history to get
n -> ... -> a -> b -> c 
-}
invertHistory :: History g a -> History g a
invertHistory history = reverse $ fmap ({-bifunctor-} first invertEdit) history
