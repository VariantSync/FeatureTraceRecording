module FeatureTraceRecording (
    featureTraceRecording
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
featureTraceRecording f0 t0 editscript contexts = reversefoldr record (f0, t0) $ zip editscript recorders
    where recorders = zipRecordingScript (fromEditScript editscript) contexts
          record = \(edit, recorder) (f_old, t_old) -> (recorder f_old t_old, run edit t_old)

data Recorder a = Recorder (FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a)
type RecordingScript a = [Recorder a]

zipRecordingScript :: RecordingScript a -> [FeatureFormula] -> [FeatureTrace a -> AST a -> FeatureTrace a]
zipRecordingScript recordings contexts 
    | length recordings == length contexts = zipWith (\(Recorder r) phi -> r phi) recordings contexts
    | otherwise = error "number of contexts does not match number of recordings"

fromEditScript :: (Show a, Eq a) => EditScript a -> RecordingScript a
fromEditScript = fmap fromEdit

fromEdit :: (Show a, Eq a) => Edit a -> Recorder a
fromEdit edit = record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

ftr_id :: Edit a -> Recorder a
ftr_id e = Recorder (\_ trace _ -> trace)

ftr_trace :: (Eq a) => Edit a -> Recorder a
ftr_trace e = Recorder (\context (FeatureTrace f) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        if member v d
            then context
            else f(v)))

ftr_ins :: (Show a, Eq a) => Edit a -> Recorder a
ftr_ins e = Recorder (\context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree
        tn = run e tree in
    FeatureTrace (\v ->
        if not $ member v d
            then f_old v
            else (case context of
                Nothing -> Nothing
                Just phi -> (if inherits phi tn d trace v then Nothing else Just phi))))

ftr_del :: (Eq a) => Edit a -> Recorder a
ftr_del e = Recorder (\context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        if not $ member v d
            then f_old v
            else (
                let pcIsNull = not $ hasvalue (pc tree trace v)
                    contextIsNull = not $ hasvalue context in
                    if contextIsNull && (not pcIsNull)
                        then Just PFalse
                        else ffand [f_old v, Just $ PNot (crack context)] -- crack is safe here because we know that context is not null
            )))

ftr_move :: (Show a, Eq a) => Edit a -> Recorder a
ftr_move e = Recorder (\context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        if hasvalue context && member v d
        then (
            let tn = run e tree
                inherits_phi = inherits (crack context) tn d trace v -- cracking phi is safe because we checked that the context has a value
                inherits_f_old = (hasvalue $ f_old v) && (inherits (crack $ f_old v) tn d trace v)
                in
            case (inherits_phi, inherits_f_old) of
                (False, False) -> ffand [f_old v, context]
                (False,  True) -> context
                ( True, False) -> f_old v
                ( True,  True) -> Nothing
        )
        else f_old v))

ftr_up :: (Eq a) => Edit a -> Recorder a
ftr_up e = Recorder (\context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        let pc_old = pc tree trace v in
        if (hasvalue context) && (hasvalue pc_old) && (member v d) && (taut $ pimplies (crack pc_old) (crack context))
        then context
        else f_old v))


{-
formula - The formula of interest that may or may not be inherited by v
tn - the AST after the edit. It contains the new nodes delta
delta - Nodes in the tree that dont have a mapping yet, i.e., their traces evaluate to Nothing.
FeatureTrace - feature trace defined on the nodes in tn except for the nodes in delta
v - The node of which we want to know if it can inherit the given formula
-}
inherits :: (Show a, Eq a) => NonNullFeatureFormula -> AST a -> Set (Node a) -> FeatureTrace a -> Node a -> Bool
inherits formula tn delta (FeatureTrace f) v =
    let al = fromList $ fmap element $ legatorAncestors tn $ tree tn v in
    (not $ disjoint delta al) -- There is an edited node in tn above v from which v can inherit the formula
    || (any
        (\a -> case f a of
            Nothing -> False
            Just m -> taut $ pimplies m formula)
        (al \\ delta)) -- There is an old ancestor that already has the mapping