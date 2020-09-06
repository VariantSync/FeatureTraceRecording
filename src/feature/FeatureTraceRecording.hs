module FeatureTraceRecording where

import Edits
import AST
import FeatureTrace
import Propositions
import NullPropositions
import Data.Set

type FTRecorder g a = FeatureFormula -> FeatureTrace g a -> AST g a -> FeatureTrace g a
type FTRecorderBuilder g a = Edit g a -> FTRecorder g a
type FTRecordingScript g a = [FTRecorder g a]

{-
Runs the given editscript on the given AST producing the returned AST.
While doing so, the given feature trace will be adapted g a 
-}
featureTraceRecording :: (Show a, Eq a) => FTRecorderBuilder g a -> FeatureTrace g a -> AST g a -> EditScript g a -> [FeatureFormula] -> (FeatureTrace g a, AST g a)
featureTraceRecording builder f0 t0 editscript contexts = last $ featureTraceRecordingWithIntermediateSteps builder f0 t0 editscript contexts

featureTraceRecordingWithIntermediateSteps :: (Show a, Eq a) => FTRecorderBuilder g a -> FeatureTrace g a -> AST g a -> EditScript g a -> [FeatureFormula] -> [(FeatureTrace g a, AST g a)]
featureTraceRecordingWithIntermediateSteps builder f0 t0 editscript contexts = scanl record (f0, t0) $ zip editscript recorders
    where recorders = zipRecordingScript (fromEditScript builder editscript) contexts
          record = \(f_old, t_old) (edit, recorder) -> (recorder f_old t_old, run edit t_old)


zipRecordingScript :: FTRecordingScript g a -> [FeatureFormula] -> [FeatureTrace g a -> AST g a -> FeatureTrace g a]
zipRecordingScript recordings contexts 
    | length recordings == length contexts = zipWith (\r phi -> r phi) recordings contexts
    | otherwise = error "number of contexts does not match number of recordings"

fromEditScript :: (Show a, Eq a) => FTRecorderBuilder g a -> EditScript g a -> FTRecordingScript g a
fromEditScript = fmap

killplain :: (Grammar g) => FTRecorder g a -> FTRecorder g a
killplain wrappee = \context f t_old -> \v ->
    if ntype v == Plain
    then Nothing
    else wrappee context f t_old v


ftr_id :: Edit g a -> FTRecorder g a
ftr_id e = \_ trace _ -> trace

ftr_trace :: (Eq a) => Edit g a -> FTRecorder g a
ftr_trace e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then context
        else f_old v

ftr_del :: (Grammar g, Eq a, Show a) => Edit g a -> FTRecorder g a
ftr_del e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if not $ member v d
        then f_old v
        else (if isnull context && not (isnull $ pc t_old f_old v)
              then Just PFalse
              else nullable_and [f_old v, nullable_not context]
        )
