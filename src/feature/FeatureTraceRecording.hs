module FeatureTraceRecording (
    featureTraceRecording,
    featureTraceRecordingWithIntermediateSteps
) where

import Edits
import Tree
import AST
import FeatureTrace
import Propositions
import NullPropositions
import SAT
import Util

import Data.Set

{-
Runs the given editscript on the given AST producing the returned AST.
While doing so, the given feature trace will be adapted a 
-}
featureTraceRecording :: (Show a, Eq a) => FeatureTrace a -> AST a -> EditScript a -> [FeatureFormula] -> (FeatureTrace a, AST a)
featureTraceRecording  f0 t0 editscript contexts = last $ featureTraceRecordingWithIntermediateSteps f0 t0 editscript contexts

featureTraceRecordingWithIntermediateSteps :: (Show a, Eq a) => FeatureTrace a -> AST a -> EditScript a -> [FeatureFormula] -> [(FeatureTrace a, AST a)]
featureTraceRecordingWithIntermediateSteps f0 t0 editscript contexts = tail $ scanl record (f0, t0) $ zip editscript recorders
    where recorders = zipRecordingScript (fromEditScript editscript) contexts
          record = \(f_old, t_old) (edit, recorder) -> (recorder f_old t_old, run edit t_old)

type Recorder a = FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a
type RecordingScript a = [Recorder a]

zipRecordingScript :: RecordingScript a -> [FeatureFormula] -> [FeatureTrace a -> AST a -> FeatureTrace a]
zipRecordingScript recordings contexts 
    | length recordings == length contexts = zipWith (\r phi -> r phi) recordings contexts
    | otherwise = error "number of contexts does not match number of recordings"

fromEditScript :: (Show a, Eq a) => EditScript a -> RecordingScript a
fromEditScript = fmap fromEdit

fromEdit :: (Show a, Eq a) => Edit a -> Recorder a
fromEdit edit = ftr_killplain $ record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

ftr_killplain :: Recorder a -> Recorder a
ftr_killplain wrappee = \context f t_old -> \v ->
    if ntype v == Plain
    then Nothing
    else wrappee context f t_old v

ftr_id :: Edit a -> Recorder a
ftr_id e = \_ trace _ -> trace

ftr_trace :: (Eq a) => Edit a -> Recorder a
ftr_trace e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then context
        else f_old v

ftr_ins :: (Show a, Eq a) => Edit a -> Recorder a
ftr_ins e = \context f_old t_old ->
    let d = delta e t_old
        t_new = run e t_old in
    \v ->
        if not $ member v d
        then f_old v
        else takeIf (\phi -> not $ willInherit phi t_new d f_old v) context

ftr_del :: (Eq a, Show a) => Edit a -> Recorder a
ftr_del e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if not $ member v d
        then f_old v
        else (
            let pcIsNull = isnull $ pc t_old f_old v
                contextIsNull = isnull context in
                if contextIsNull && not pcIsNull
                then Just PFalse
                else nullable_and [f_old v, Just $ PNot (assure context)] -- assure is safe here because we know that context is not null
        )

{-
Moving is ambigous.
Here, we say that existing traces should not be kept (except for simplification).
This could be unwanted but there is no way to differentiate both cases.
When removing existing traces, considering moves explicitly is pointless.
Given that moves are harder to detect in general, not keeping traces could actually be a more consistent, reliable, and comprehensible experience for the user.
Such a conclusion would also be valid, though. (That there is no need for handling moves explicitly).
-}
ftr_move :: (Show a, Eq a) => Edit a -> Recorder a
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

ftr_up :: (Eq a, Show a) => Edit a -> Recorder a
ftr_up e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        let pc_old = pc t_old f_old v in
        if (notnull context) && (notnull pc_old) && (member v d) && (taut $ pimplies (assure pc_old) (assure context))
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

simple_ftr_ins :: (Show a, Eq a) => Edit a -> Recorder a
simple_ftr_ins e = \context f_old t_old ->
    let d = delta e t_old
        t_new = run e t_old in
    \v ->
        if member v d
        then context
        else f_old v

simple_ftr_move :: (Show a, Eq a) => Edit a -> Recorder a
simple_ftr_move e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then nullable_and [f_old v, context]
        else f_old v