module FeatureTraceRecording where

import Edits
import AST
import FeatureTrace
import Propositions
import NullPropositions
import Data.Set

type RecordingFunction g a = FeatureFormula -> FeatureTrace g a -> AST g a -> FeatureTrace g a
type FeatureTraceRecordingAlgorithm g a = Edit g a -> RecordingFunction g a

{-
Runs the given feature trace recording algorithm.
Arguments are:
1. chosen feature trace recording algorithm (for example defaultFeatureTraceRecording)
2. initial feature trace (for example FeatureTrace.emptyTrace)
3. initial source code as AST
4. edit script (i.e., the sequence of edits applied to the initial AST upon which feature traces should be recorded)
5. lost of feature contexts, one feature context for each edit
-}
featureTraceRecording :: (Show a, Eq a) => FeatureTraceRecordingAlgorithm g a -> FeatureTrace g a -> AST g a -> EditScript g a -> [FeatureFormula] -> (FeatureTrace g a, AST g a)
featureTraceRecording algorithm f0 t0 editscript contexts = last $ featureTraceRecordingWithIntermediateSteps algorithm f0 t0 editscript contexts

{-
The same as featureTraceRecording but also returns all intermediate results.
-}
featureTraceRecordingWithIntermediateSteps :: (Show a, Eq a) => FeatureTraceRecordingAlgorithm g a -> FeatureTrace g a -> AST g a -> EditScript g a -> [FeatureFormula] -> [(FeatureTrace g a, AST g a)]
featureTraceRecordingWithIntermediateSteps algorithm f0 t0 editscript contexts =
    scanl record (f0, t0) $ zip editscript $ zipWith ($) (algorithm <$> editscript) contexts
    where record = \(f_old, t_old) (edit, recorder) -> (recorder f_old t_old, run edit t_old)

{-
Algorithm 1 from the paper.
-}
defaultFeatureTraceRecording :: (Grammar g, Show a, Eq a) => FeatureTraceRecordingAlgorithm g a
defaultFeatureTraceRecording edit =
    removeTheRedundanciesWeIntroduced edit $
    nullifyMandatory $
    record edit
    where
    record = case edittype edit of
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
nullifyMandatory wrappee = \context f_old t_old -> \v ->
    if optionaltype v == Mandatory
    then Nothing
    else wrappee context f_old t_old v

-- This is called 'simplify' in the paper.
removeTheRedundanciesWeIntroduced :: (Grammar g, Eq a, Show a) => Edit g a -> RecordingFunction g a -> RecordingFunction g a
removeTheRedundanciesWeIntroduced edit wrappee = \context f_old t_old ->
    let f_new = wrappee context f_old t_old
        t_new = run edit t_old in
        FeatureTrace.simplify f_new t_new

{-
Feature trace recording for identity edit:
When nothing is changed, nothing has to be recorded.
-}
ftr_id :: Edit g a -> RecordingFunction g a
ftr_id e = \_ trace _ -> trace

{-
Feature trace recording on an identity edit with non-empty delta.
This function allows changing feature traces manually (i.e., without actual code changes).
Please have a look at the function Edits.edit_trace_only in src/tree/Edits.hs.
-}
ftr_trace :: (Eq a) => Edit g a -> RecordingFunction g a
ftr_trace e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then context
        else f_old v

{- The recording functions from Section 5 in the paper. -}

ftr_ins :: (Show a, Eq a) => Edit g a -> RecordingFunction g a
ftr_ins e = \context f_old t_old ->
    \v ->
        if member v $ delta e t_old
        then context
        else f_old v

ftr_del :: (Grammar g, Eq a, Show a) => Edit g a -> RecordingFunction g a
ftr_del e = \context f_old t_old ->
    \v ->
        if not $ member v $ delta e t_old
        then f_old v
        else (if isnull context && not (isnull $ pc t_old f_old v)
              then Just PFalse
              {-
              Due to our logical operators with null,
              three of the cases of R_del in the paper can actually be collapsed
              into this single formula.
              -}
              else nullable_and [f_old v, nullable_not context] 
        )

ftr_move :: (Show a, Eq a) => Edit g a -> RecordingFunction g a
ftr_move e = \context f_old t_old ->
    \v ->
        if member v $ delta e t_old
        then nullable_and [f_old v, context]
        else f_old v

ftr_up :: (Eq a, Show a) => Edit g a -> RecordingFunction g a
ftr_up e = \context f_old t_old ->
    \v ->
        if (notnull context) && (member v $ delta e t_old)
        then context
        else f_old v