{- |
Description: Implementation of feature trace recording as proposed in the paper.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Implementation of feature trace recording as proposed in the paper (Section 4.2).
-}
module DefaultFeatureTraceRecording where

import Grammar
import Edits
import AST
import FeatureTrace
import FeatureTraceRecording
import Logic
import NullPropositions
import Data.Set
import ListUtil

{- |
This is the default implementation of feature trace recording as proposed in our paper.
For each type of edit, we choose one of the four recording functions from R_ins, R_del, R_mov, and R_up.
Next to these four functions, we support two more default recording function in this library, 'r_id' and 'r_trace'.
We use 'r_id' for technical reasons (e.g., folds, printing) and it does not affect the mappings.
We use 'r_trace' to manually change the feature mappings of a set of nodes without changing the source code.
-}
defaultFeatureTraceRecording :: (Grammar g, Show a, Eq a) => FeatureTraceRecording g a
defaultFeatureTraceRecording typeOfEdit =
    removeTheRedundanciesWeIntroduced $
    nullifyMandatory $
    case typeOfEdit of
        Identity -> r_id
        TraceOnly -> r_trace
        Insert -> r_ins
        Delete -> r_del
        Move -> r_move
        Update -> r_up

{- |
Sets the feature mapping of all mandatory AST nodes to /null/.
-}
nullifyMandatory :: (Grammar g) => RecordingFunction g a -> RecordingFunction g a
nullifyMandatory wrappee = \redit version -> \v ->
    if optionaltype v == Mandatory
    then Nothing
    else wrappee redit version v

{- |
This simplifies feature traces with respect to presence conditions.
See 'FeatureTrace.simplifyFeatureTrace' and 'Simplify.removeRedundancy' for further information.
-}
removeTheRedundanciesWeIntroduced :: (Grammar g, Eq a, Show a) => RecordingFunction g a -> RecordingFunction g a
removeTheRedundanciesWeIntroduced wrappee = \redit@(edit, _) version@(_, t_old) ->
    let f_new = wrappee redit version
        t_new = run edit t_old
        d = delta edit t_old in
        FeatureTrace.simplifyFeatureTraceOfNodes f_new t_new d

{- |
Feature trace recording for identity edit:
When nothing is changed, nothing has to be recorded.
-}
r_id :: RecordingFunction g a
r_id _ (f_old, _) = f_old

{- |
Feature trace recording on an identity edit with non-empty delta.
This function allows changing feature traces manually (i.e., without actual source code changes).
Have a look at 'Edits.edit_trace_only' for further information.
-}
r_trace :: (Eq a) => RecordingFunction g a
r_trace (edit, context) (f_old, t_old) =
    let d = delta edit t_old in
    \v ->
        if member v d
        then context
        else f_old v

{- The recording functions from Section 4 in the paper. -}

-- | Equation 2 in the paper: R_ins records feature traces upon insertions. 
r_ins :: (Show a, Eq a) => RecordingFunction g a
r_ins (edit, context) (f_old, t_old) =
    \v ->
        if member v $ delta edit t_old
        then context
        else f_old v

-- | Equation 2 in the paper: R_del records feature traces upon deletions. 
r_del :: (Grammar g, Eq a, Show a) => RecordingFunction g a
r_del (edit, context) (f_old, t_old) =
    \v ->
        let pcv = pc t_old f_old v in
        if not $ member v $ delta edit t_old
        then f_old v
        else (if isnull context && not (isnull pcv)
              then lfalse
              {-
              Due to our logical operators with null,
              two of the cases of R_del in the paper can actually be collapsed
              into this single formula.
              -}
              else land [pcv, lnot context] 
        )

-- | Equation 3 in the paper: R_move records feature traces upon moves. 
r_move :: (Show a, Eq a) => RecordingFunction g a
r_move (edit, context) (f_old, t_old) =
    \v ->
        if member v $ delta edit t_old
        then land [f_old v, context]
        else f_old v

-- | Equation 5 in the paper: R_up records feature traces upon updates. 
r_up :: (Eq a, Show a) => RecordingFunction g a
r_up (edit, context) (f_old, t_old) =
    \v ->
        if (notnull context) && (member v $ delta edit t_old)
        then context
        else f_old v
