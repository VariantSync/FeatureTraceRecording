module DefaultFeatureTraceRecording where

import Grammar
import Edits
import StructuralElement
import FeatureTrace
import FeatureTraceRecording
import Logic
import NullPropositions
import Data.Set
import ListUtil

{-
Algorithm 1 from the paper.
-}
defaultFeatureTraceRecording :: (StructuralElement s) => FeatureTraceRecording s
defaultFeatureTraceRecording typeOfEdit =
    -- removeTheRedundanciesWeIntroduced $
    -- nullifyMandatory $
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
nullifyMandatory :: (StructuralElement s) => RecordingFunction s -> RecordingFunction s
nullifyMandatory wrappee = \redit version -> \v ->
    if se_canBeAnnotated v
    then wrappee redit version v
    else Nothing

-- This is called 'simplify' in the paper.
removeTheRedundanciesWeIntroduced :: (StructuralElement s) => RecordingFunction s -> RecordingFunction s
removeTheRedundanciesWeIntroduced wrappee = \redit@(edit, _) version@(_, t_old) ->
    let f_new = wrappee redit version
        t_new = run edit t_old
        d = delta edit t_old in
        FeatureTrace.simplifyFeatureTraceOfNodes f_new t_new d

{-
Feature trace recording for identity edit:
When nothing is changed, nothing has to be recorded.
-}
ftr_id :: RecordingFunction s
ftr_id _ (f_old, _) = f_old

{-
Feature trace recording on an identity edit with non-empty delta.
This function allows changing feature traces manually (i.e., without actual code changes).
Please have a look at the function Edits.edit_trace_only in src/tree/Edits.hs.
-}
ftr_trace :: (StructuralElement s) => RecordingFunction s
ftr_trace (edit, context) (f_old, t_old) =
    let d = delta edit t_old in
    \v ->
        if member v d
        then context
        else f_old v

{- The recording functions from Section 5 in the paper. -}

ftr_ins :: (StructuralElement s) => RecordingFunction s
ftr_ins (edit, context) (f_old, t_old) =
    \v ->
        if member v $ delta edit t_old
        then context
        else f_old v

ftr_del :: (StructuralElement s) => RecordingFunction s
ftr_del (edit, context) (f_old, t_old) =
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

ftr_move :: (StructuralElement s) => RecordingFunction s
ftr_move (edit, context) (f_old, t_old) =
    \v ->
        -- if member v $ delta edit t_old
        -- then land [f_old v, context]
        -- else 
            f_old v

ftr_up :: (StructuralElement s) => RecordingFunction s
ftr_up (edit, context) (f_old, t_old) =
    \v ->
        if (notnull context) && (member v $ delta edit t_old)
        then context
        else f_old v
