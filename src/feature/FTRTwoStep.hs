module FTRTwoStep where

import FeatureTraceRecording
import FeatureTrace
import Propositions
import NullPropositions
import Simplify
import Edits
import Data.Set

builder :: (Show a, Eq a) => FTRecorderBuilder a
builder edit = removeTheRedundanciesWeIntroduced edit $ killplain $ record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

removeTheRedundanciesWeIntroduced :: (Eq a, Show a) => Edit a -> FTRecorder a -> FTRecorder a
removeTheRedundanciesWeIntroduced edit wrappee =
    \context f t_old ->
        let f_new = wrappee context f t_old
            t_new = run edit t_old in
        \v -> case (pc_parentpart t_new f_new v, f_new v) of
                (Just p, Just f) -> case removeRedundancy p f of
                    PTrue -> Nothing
                    p -> Just p
                _ -> f_new v >>= \f -> Just $ simplify f

ftr_ins :: (Show a, Eq a) => Edit a -> FTRecorder a
ftr_ins e = \context f_old t_old ->
    let d = delta e t_old
        t_new = run e t_old in
    \v ->
        if member v d
        then context
        else f_old v

ftr_move :: (Show a, Eq a) => Edit a -> FTRecorder a
ftr_move e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then nullable_and [f_old v, context]
        else f_old v

ftr_up :: (Eq a, Show a) => Edit a -> FTRecorder a
ftr_up e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if (notnull context) && (member v d)
        then context
        else f_old v