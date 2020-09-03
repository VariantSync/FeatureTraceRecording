module FeatureTraceRecording where

import Edits
import AST
import FeatureTrace
import Propositions
import NullPropositions
import Data.Set

type FTRecorder a = FeatureFormula -> FeatureTrace a -> AST a -> FeatureTrace a
type FTRecorderBuilder a = Edit a -> FTRecorder a
type FTRecordingScript a = [FTRecorder a]

{-
Runs the given editscript on the given AST producing the returned AST.
While doing so, the given feature trace will be adapted a 
-}
featureTraceRecording :: (Show a, Eq a) => FTRecorderBuilder a -> FeatureTrace a -> AST a -> EditScript a -> [FeatureFormula] -> (FeatureTrace a, AST a)
featureTraceRecording builder f0 t0 editscript contexts = last $ featureTraceRecordingWithIntermediateSteps builder f0 t0 editscript contexts

featureTraceRecordingWithIntermediateSteps :: (Show a, Eq a) => FTRecorderBuilder a -> FeatureTrace a -> AST a -> EditScript a -> [FeatureFormula] -> [(FeatureTrace a, AST a)]
featureTraceRecordingWithIntermediateSteps builder f0 t0 editscript contexts = tail $ scanl record (f0, t0) $ zip editscript recorders
    where recorders = zipRecordingScript (fromEditScript builder editscript) contexts
          record = \(f_old, t_old) (edit, recorder) -> (recorder f_old t_old, run edit t_old)


zipRecordingScript :: FTRecordingScript a -> [FeatureFormula] -> [FeatureTrace a -> AST a -> FeatureTrace a]
zipRecordingScript recordings contexts 
    | length recordings == length contexts = zipWith (\r phi -> r phi) recordings contexts
    | otherwise = error "number of contexts does not match number of recordings"

fromEditScript :: (Show a, Eq a) => FTRecorderBuilder a -> EditScript a -> FTRecordingScript a
fromEditScript = fmap

killplain :: FTRecorder a -> FTRecorder a
killplain wrappee = \context f t_old -> \v ->
    if ntype v == Plain
    then Nothing
    else wrappee context f t_old v


ftr_id :: Edit a -> FTRecorder a
ftr_id e = \_ trace _ -> trace

ftr_trace :: (Eq a) => Edit a -> FTRecorder a
ftr_trace e = \context f_old t_old ->
    let d = delta e t_old in
    \v ->
        if member v d
        then context
        else f_old v

ftr_del :: (Eq a, Show a) => Edit a -> FTRecorder a
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
