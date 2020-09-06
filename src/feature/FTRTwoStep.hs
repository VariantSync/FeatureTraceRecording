module FTRTwoStep where

import FeatureTraceRecording
import FeatureTrace
import AST (Grammar)
import Propositions
import NullPropositions
import Simplify
import Edits
import Data.Set

builder :: (Grammar g, Show a, Eq a) => FTRecorderBuilder g a
builder edit = removeTheRedundanciesWeIntroduced edit $ killplain $ record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

removeTheRedundanciesWeIntroduced :: (Grammar g, Eq a, Show a) => Edit g a -> FTRecorder g a -> FTRecorder g a
removeTheRedundanciesWeIntroduced edit wrappee = \context f_old t_old ->
    let f_new = wrappee context f_old t_old
        t_new = run edit t_old in
        FeatureTrace.simplify f_new t_new

ftr_ins :: (Show a, Eq a) => Edit g a -> FTRecorder g a
ftr_ins e = \context f_old t_old ->
    let d = delta e t_old
        t_new = run e t_old in
    \v ->
        if member v d
        then context
        else f_old v

ftr_move :: (Show a, Eq a) => Edit g a -> FTRecorder g a
ftr_move e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then nullable_and [f_old v, context]
        else f_old v

ftr_up :: (Eq a, Show a) => Edit g a -> FTRecorder g a
ftr_up e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if (notnull context) && (member v d)
        then context
        else f_old v