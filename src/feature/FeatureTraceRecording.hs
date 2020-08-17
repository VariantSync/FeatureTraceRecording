module FeatureTraceRecording where

import Edits
import AST
import FeatureTrace

import Control.Monad.State

data Recording a = Recording {record :: FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a}

ftr :: Edit a -> Recording a
ftr edit = record edit where
    record = case edittype edit of
        Identity -> ftr_id
        Insert -> ftr_ins
    -- Delete -> ftr_del
    -- Move -> ftr_move
    -- Update -> ftr_up
    -- delta = []

ftr_id :: Edit a -> Recording a
ftr_id e = Recording {record = \_ trace _ -> trace}

ftr_ins :: Edit a -> Recording a
ftr_ins e = Recording {record = \context trace tree ->
    trace
    }