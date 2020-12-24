module DefaultFeatureTraceRecording where

import Grammar
import Edits
import AST
import Feature
import FeatureTrace
import FeatureTraceRecording
import Logic
import NullPropositions
import Data.Set
import ListUtil

{-
Algorithm 1 from the paper.
-}
defaultFeatureTraceRecording :: (Grammar g, Show a, Eq a) => FeatureTraceRecording g a
defaultFeatureTraceRecording typeOfEdit =
    removeTheRedundanciesWeIntroduced . nullifyMandatory $
    case typeOfEdit of
        Identity -> ftr_id
        TraceOnly -> ftr_trace
        Insert -> ftr_ins
        Delete -> ftr_del
        Move -> ftr_move
        Update -> ftr_up

{-
Sets the feature trace of all mandatory AST nodes to null.
-}
nullifyMandatory :: (Grammar g) => RecordingFunction g a -> RecordingFunction g a
nullifyMandatory wrappee = RecordingFunction {    
    runRecording = \redit version -> \v ->
        if optionaltype v == Mandatory
        then Nothing
        else runRecording wrappee redit version v,
    {-
    This is fine because there never can be an (actual) edit only affecting mandatory nodes.
    As recording functions aren't supposed to assign formulas to mandatory nodes in the first place,
    they shouldn't be confused when all mandatory nodes don't have a trace afterwards.
    -}
    reverseEngineerFeatureContext = reverseEngineerFeatureContext wrappee
}

-- This is called 'simplify' in the paper.
removeTheRedundanciesWeIntroduced :: (Grammar g, Eq a, Show a) => RecordingFunction g a -> RecordingFunction g a
removeTheRedundanciesWeIntroduced wrappee = RecordingFunction {
    runRecording = \redit@(edit, _) version@(_, t_old) ->
        let f_new = runRecording wrappee redit version
            t_new = run edit t_old in
            FeatureTrace.simplify f_new t_new,
    reverseEngineerFeatureContext = reverseEngineerFeatureContext wrappee
}

{-
Feature trace recording for identity edit:
When nothing is changed, nothing has to be recorded.
-}
ftr_id :: RecordingFunction g a
ftr_id = RecordingFunction {
    runRecording = \_ (f_old, _) -> f_old,
    reverseEngineerFeatureContext = \_ _ _ -> Any
}

distinctFeatureTraces :: (Grammar g) => Set (Node g a) -> FeatureTrace g a -> [FeatureFormula]
distinctFeatureTraces nodes trace = removeDuplicates $ trace <$> (Prelude.filter (\n -> Mandatory /= optionaltype n) $ toList nodes)

{-
Feature trace recording on an identity edit with non-empty delta.
This function allows changing feature traces manually (i.e., without actual code changes).
Please have a look at the function Edits.edit_trace_only in src/tree/Edits.hs.
-}
ftr_trace :: (Eq a, Grammar g) => RecordingFunction g a
ftr_trace = RecordingFunction {
    runRecording = \(edit, context) (f_old, t_old) ->
        let d = delta edit t_old in
        \v ->
            if member v d
            then context
            else f_old v,
    {-
    It is actually not that easy.
    The problem is the removeRedundancies step.
    Even though ftr_trace and ftr_ins assign the context to all nodes in delta,
    these nodes can have different traces after the simplification.
    Example:
    Given the tree:
        root
        node1
            node2
        node3
            node4
    with trace f_old
        f_old(node1) = A
        f_old(node3) = C.
    Assume we run ftr_trace with context = A and B and delta = {node2, node4}.
    Then we get f_new with
        f_new node1 = A
        f_new node2 = A and B
        f_new node3 = C
        f_new node4 = A and B.
    but after the simplification we get
        (simp f_new) node1 = A
        (simp f_new) node2 = B
        (simp f_new) node3 = C
        (simp f_new) node4 = A and B.
    We then have the case delta = {node2, node4} but node2 and node4
        - have different traces
        - have non-equivalent traces
        - have non-equivalent presence conditions.
    Maybe we can grab the strongest value of (simp f_new) on delta?
    And I guess we have to assume that the given f_new was indeed recorded with ftr_trace on the given edit.

    However, the simplification is not part of this recording function.
    It is part of the encapsulating FTR.
    So inverting this recording function is actually independent from the simplification.
    -}
    reverseEngineerFeatureContext = \edit (f_old, t_old) f_new ->
        let d = delta edit t_old in
        case distinctFeatureTraces d f_new of
            [] -> None "The given edit is a noop"
            [x] -> Exactly x
            (x:xs) -> None "The given edit does not fit this recording function" -- wrong assumption... simplify destroys things
}

{- The recording functions from Section 5 in the paper. -}

ftr_ins :: (Show a, Eq a) => RecordingFunction g a
ftr_ins = RecordingFunction {
    runRecording = \(edit, context) (f_old, t_old) ->
    \v ->
        if member v $ delta edit t_old
        then context
        else f_old v,
    reverseEngineerFeatureContext = \_ _ _ -> None "Not implemented"
}

ftr_del :: (Grammar g, Eq a, Show a) => RecordingFunction g a
ftr_del = RecordingFunction {
    runRecording = \(edit, context) (f_old, t_old) ->
    \v ->
        if not $ member v $ delta edit t_old
        then f_old v
        else (if isnull context && not (isnull $ pc t_old f_old v)
              then lfalse
              {-
              Due to our logical operators with null,
              three of the cases of R_del in the paper can actually be collapsed
              into this single formula.
              -}
              else land [f_old v, lnot context] 
        ),
    reverseEngineerFeatureContext = \_ _ _ -> None "Not implemented"
}

ftr_move :: (Show a, Eq a) => RecordingFunction g a
ftr_move = RecordingFunction {
    runRecording = \(edit, context) (f_old, t_old) ->
    \v ->
        if member v $ delta edit t_old
        then land [f_old v, context]
        else f_old v,
    reverseEngineerFeatureContext = \_ _ _ -> None "Not implemented"
}

ftr_up :: (Eq a, Show a) => RecordingFunction g a
ftr_up = RecordingFunction {
    runRecording = \(edit, context) (f_old, t_old) ->
    \v ->
        if (notnull context) && (member v $ delta edit t_old)
        then context
        else f_old v,
    reverseEngineerFeatureContext = \_ _ _ -> None "Not implemented"
}
