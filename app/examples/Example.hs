{- |
Description: Module for creating demos of feature trace recording.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module for creating demos of feature trace recording.
-}
module Example where

import AST ( AST )
import Edits ( EditScript )
import Grammar
import Feature
import FeatureTrace
import FeatureTraceRecording
import DefaultFeatureTraceRecording
import FeatureColour (FeatureFormulaColourPalette)
import Control.Monad.State ( State, evalState )
import UUID

{- | An 'Example' represents a single demo showcase of feature trace recording.

- @m@: Monad defining colours which is used for printing an examples output.
- @g@: Grammar of the example (e.g., if the examples shows the development of Java or C++ or Haskell source code).
- @a@: Value type of the artefacts in the 'AST's (e.g., @String@).
-}
data Example m g a = Example {
    -- | The name of the example to identify it.
    name :: String,
    -- | The version of the source code (as 'AST') and 'FeatureTrace's when the recording is started.
    startVersion :: Version g a,
    -- | The history of edits upon which to record feature traces when applied to 'startVersion'.
    history :: History g a,
    -- | A colour scheme for displaying features and feature formula.
    colours :: FeatureFormulaColourPalette m
}

-- | Runs and example with the given 'FeatureTraceRecording' implementation, yielding a list of all intermediate versions.
-- The last element in the returned list is the 'AST' and 'FeatureTrace's to which the 'startVersion' was changed when aplying all edits in 'history' of the 'Example'.
runExample :: (Grammar g, Show a, Eq a) => FeatureTraceRecording g a -> Example m g a -> [Version g a]
runExample ftr example = runFTRWithIntermediateSteps ftr (startVersion example) (history example)

-- | Runs the given example with the default implementation of 'FeatureTraceRecording' ('defaultFeatureTraceRecording').
-- Is equivalent to @runExample defaultFeatureTraceRecording@.
runExampleWithDefaultFTR :: (Grammar g, Show a, Eq a) => Example m g a -> [Version g a]
runExampleWithDefaultFTR = runExample defaultFeatureTraceRecording

-- | Evaluates all 'UUID's in the example, starting with @0@.
-- Use this after building an 'AST' whose 'Node's yet have to be assigned 'UUID's.
finalizeExample :: State UUID (Example m g a) -> Example m g a
finalizeExample ex = evalState ex 0
