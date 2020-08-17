module FeatureTraceRecording where

import Edits
import Tree
import AST
import FeatureTrace

import Data.Set

data Recording a = Recording {record :: FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a}

inherits :: (Show a, Eq a) => NonNullFeatureFormula -> AST a -> Set (Node a) -> FeatureTrace a -> Node a -> Bool
inherits formula tn delta (FeatureTrace f) v =
    (not $ disjoint delta $ fromList $ fmap element $ legatorAncestors tn (tree tn v)) -- There is an edited node in tn above v from which v can inherit the formula
    || False -- We need to solve sat queries here

ftr :: (Show a, Eq a) => Edit a -> Recording a
ftr edit = record edit where
    record = case edittype edit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
    -- Delete -> ftr_del
    -- Move -> ftr_move
    -- Update -> ftr_up

ftr_id :: Edit a -> Recording a
ftr_id e = Recording {record = \_ trace _ -> trace}

ftr_trace :: (Eq a) => Edit a -> Recording a
ftr_trace e = Recording {record = \context (FeatureTrace f) tree -> let d = delta e tree in FeatureTrace (\v ->
    if member v d
    then context
    else f(v)
)}

ftr_ins :: (Show a, Eq a) => Edit a -> Recording a
ftr_ins e = Recording {record = \context trace@(FeatureTrace f) tree -> let d = delta e tree in FeatureTrace (\v ->
    if not $ member v d
    then f(v)
    else (case context of
        Just phi -> (if inherits phi tree d trace v then Nothing else Just phi)
        Nothing -> Nothing
))}