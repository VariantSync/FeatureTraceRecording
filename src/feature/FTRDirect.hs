module FTRDirect where

import FeatureTraceRecording
import Edits
import Tree
import AST
import FeatureTrace
import Propositions
import NullPropositions
import SAT
import Data.Set
import Util

build :: (Eq a, Show a) => FTRecorderBuilder a
build edit = killplain $ record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

ftr_ins :: (Show a, Eq a) => Edit a -> FTRecorder a
ftr_ins e = \context f_old t_old ->
    let d = delta e t_old
        t_new = run e t_old in
    \v ->
        if not $ member v d
        then f_old v
        else takeIf (\phi -> not $ willInherit phi t_new d f_old v) context

{-
Moving is ambigous.
Here, we say that existing traces should not be kept (except for simplification).
This could be unwanted but there is no way to differentiate both cases.
When removing existing traces, considering moves explicitly is pointless.
Given that moves are harder to detect in general, not keeping traces could actually be a more consistent, reliable, and comprehensible experience for the user.
Such a conclusion would also be valid, though. (That there is no need for handling moves explicitly).
-}
ftr_move :: (Show a, Eq a) => Edit a -> FTRecorder a
ftr_move e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if notnull context && member v d
        then (
            let t_new = run e t_old in
            nullable_and [
                takeIf (\traceof_v -> not $ willInherit2 traceof_v t_new f_old v) (f_old v),
                takeIf (\phi -> not $ willInherit phi t_new d f_old v) context]
        )
        else f_old v

ftr_up :: (Eq a, Show a) => Edit a -> FTRecorder a
ftr_up e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        let pc_old = pc t_old f_old v in
        if (notnull context) && (member v d) && (isnull pc_old || not (taut $ pimplies (assure pc_old) (assure context)))
        then context
        else f_old v


{-
formula - The formula of interest that may or may not be inherited by v
tn - the AST after the edit. It contains the new nodes delta.
delta - Nodes in the tree that dont have a mapping yet, i.e., their traces evaluate to Nothing.
FeatureTrace - feature trace defined on the nodes in tn except for the nodes in delta
v - The node of which we want to know if it can inherit the given formula
-}
willInherit :: (Show a, Eq a) => NonNullFeatureFormula -> AST a -> Set (Node a) -> FeatureTrace a -> Node a -> Bool
willInherit formula t_new delta f v =
    let al = fromList $ fmap element $ legatorAncestors t_new $ tree t_new v in
    (not $ disjoint delta al) -- There is an edited node in tn above v from which v can inherit the formula
    || (any
        (\a -> case f a of
            Nothing -> False
            Just m -> taut $ pimplies m formula)
        (al \\ delta)) -- There is an old ancestor that already has the mapping

willInherit2 :: (Show a, Eq a) => NonNullFeatureFormula -> AST a-> FeatureTrace a -> Node a -> Bool
willInherit2 formula t_new f v =
    let al = fromList $ fmap element $ legatorAncestors t_new $ tree t_new v in
    any (\a -> case f a of
        Nothing -> False
        Just m -> taut $ pimplies m formula) al
