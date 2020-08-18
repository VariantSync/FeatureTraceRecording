module FeatureTraceRecording where

import Edits
import Tree
import AST
import FeatureTrace
import Propositions
import NullPropositions
import SAT
import Util

import Data.Set

data Recording a = Recording {record :: FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a}

ftr :: (Show a, Eq a) => Edit a -> Recording a
ftr edit = record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

ftr_id :: Edit a -> Recording a
ftr_id e = Recording {record = \_ trace _ -> trace}

ftr_trace :: (Eq a) => Edit a -> Recording a
ftr_trace e = Recording {record = \context (FeatureTrace f) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        if member v d
            then context
            else f(v)
)}

ftr_ins :: (Show a, Eq a) => Edit a -> Recording a
ftr_ins e = Recording {record = \context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree
        tn = run e tree in
    FeatureTrace (\v ->
        if not $ member v d
            then f_old v
            else (case context of
                Nothing -> Nothing
                Just phi -> (if inherits phi tn d trace v then Nothing else Just phi)
))}

ftr_del :: (Eq a) => Edit a -> Recording a
ftr_del e = Recording {record = \context trace@(FeatureTrace f_old) tree ->
    let d = delta e tree in
    FeatureTrace (\v ->
        if not $ member v d
            then f_old v
            else (
                let pcIsNull = hasvalue (pc tree trace v)
                    contextIsNull = hasvalue context in
                    if contextIsNull && (not pcIsNull)
                        then Just PFalse
                        else ffand [f_old v, Just $ PNot (crack context)] -- crack is safe here because we know that context is not null
            )
)}

-- TODO
ftr_move :: Edit a -> Recording a
ftr_move = ftr_id

-- TODO
ftr_up :: Edit a -> Recording a
ftr_up = ftr_id


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