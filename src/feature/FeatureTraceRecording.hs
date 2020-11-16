module FeatureTraceRecording where

import Edits
import AST
import Feature
import FeatureTrace
import Logic
import Propositions
import NullPropositions
import Data.Set

type FeatureContext = FeatureFormula
type RecordedEdit g a = (Edit g a, FeatureContext)
type RecordedEditScript g a = [RecordedEdit g a]
type RecordingFunction g a = RecordedEdit g a -> FeatureTrace g a -> AST g a -> FeatureTrace g a
type FeatureTraceRecordingAlgorithm g a = EditType -> RecordingFunction g a

{-
Runs the given feature trace recording algorithm.
Arguments are:
1. chosen feature trace recording algorithm (for example defaultFeatureTraceRecording)
2. initial feature trace (for example FeatureTrace.emptyTrace)
3. initial source code as AST
4. edit script (i.e., the sequence of edits applied to the initial AST upon which feature traces should be recorded) with the corresponding feature contexts
-}
featureTraceRecording :: (Show a, Eq a) => FeatureTraceRecordingAlgorithm g a -> FeatureTrace g a -> AST g a -> RecordedEditScript g a -> (FeatureTrace g a, AST g a)
featureTraceRecording algorithm f0 t0 reditscript = last $ featureTraceRecordingWithIntermediateSteps algorithm f0 t0 reditscript

{-
The same as featureTraceRecording but also returns all intermediate results.
-}
featureTraceRecordingWithIntermediateSteps :: (Show a, Eq a) => FeatureTraceRecordingAlgorithm g a -> FeatureTrace g a -> AST g a -> RecordedEditScript g a -> [(FeatureTrace g a, AST g a)]
featureTraceRecordingWithIntermediateSteps algorithm f0 t0 reditscript =
    scanl record (f0, t0) reditscript
    where record (f_old, t_old) recordedEdit@(edit, _) = ((algorithm $ edittype edit) recordedEdit f_old t_old, run edit t_old)

{-
Algorithm 1 from the paper.
-}
defaultFeatureTraceRecording :: (Grammar g, Show a, Eq a) => FeatureTraceRecordingAlgorithm g a
defaultFeatureTraceRecording typeOfEdit =
    removeTheRedundanciesWeIntroduced $
    nullifyMandatory $
    case typeOfEdit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

{-
Sets the feature trace of all mandatory AST nodes to null.
-}
nullifyMandatory :: (Grammar g) => RecordingFunction g a -> RecordingFunction g a
nullifyMandatory wrappee = \redit f_old t_old -> \v ->
    if optionaltype v == Mandatory
    then Nothing
    else wrappee redit f_old t_old v

-- This is called 'simplify' in the paper.
removeTheRedundanciesWeIntroduced :: (Grammar g, Eq a, Show a) => RecordingFunction g a -> RecordingFunction g a
removeTheRedundanciesWeIntroduced wrappee = \redit@(edit, context) f_old t_old ->
    let f_new = wrappee redit f_old t_old
        t_new = run edit t_old in
        FeatureTrace.simplify f_new t_new

{-
Feature trace recording for identity edit:
When nothing is changed, nothing has to be recorded.
-}
ftr_id :: RecordingFunction g a
ftr_id _ trace _ = trace

{-
Feature trace recording on an identity edit with non-empty delta.
This function allows changing feature traces manually (i.e., without actual code changes).
Please have a look at the function Edits.edit_trace_only in src/tree/Edits.hs.
-}
ftr_trace :: (Eq a) => RecordingFunction g a
ftr_trace (edit, context) f_old t_old =
    let d = delta edit t_old in
    \v ->
        if member v d
        then context
        else f_old v

{- The recording functions from Section 5 in the paper. -}

ftr_ins :: (Show a, Eq a) => RecordingFunction g a
ftr_ins (edit, context) f_old t_old =
    \v ->
        if member v $ delta edit t_old
        then context
        else f_old v

ftr_del :: (Grammar g, Eq a, Show a) => RecordingFunction g a
ftr_del (edit, context) f_old t_old =
    \v ->
        if not $ member v $ delta edit t_old
        then f_old v
        else (if isnull context && not (isnull $ pc t_old f_old v)
              then lfalse
              {-
              Due to our logical operators with null,
              three of the cases of R_del in the paper can actually be collapsed
              into this single formula.
              -}
              else land [f_old v, lnot context] 
        )

ftr_move :: (Show a, Eq a) => RecordingFunction g a
ftr_move (edit, context) f_old t_old =
    \v ->
        if member v $ delta edit t_old
        then land [f_old v, context]
        else f_old v

ftr_up :: (Eq a, Show a) => RecordingFunction g a
ftr_up (edit, context) f_old t_old =
    \v ->
        if (notnull context) && (member v $ delta edit t_old)
        then context
        else f_old v