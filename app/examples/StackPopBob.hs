﻿module StackPopBob where

import StackPopAlice ( feature_ImmutableStack, example )

import UUID ( UUID )
import Example ( Example(..) )
import Edits ( Edit(delta), foldEditScript, edit_trace_only )
import Propositions ( PropositionalFormula(..) )
import SimpleJava ( SimpleJavaGrammar )
import FeatureColour
import FeatureTrace

import System.Terminal
import Control.Monad.State ( State ) 

featureColourPalette :: (MonadColorPrinter m) => FeatureFormulaColourPalette m -> FeatureFormulaColourPalette m
featureColourPalette fallback formula 
    | formula == (Just $ PVariable $ feature_ImmutableStack) = magenta
    | otherwise = fallback formula

example :: MonadColorPrinter m => State UUID (Example m SimpleJavaGrammar String)
example =
    StackPopAlice.example
    >>= \alice ->
        let numEditsToSynchronise = 2
            deleteThatUpdatesBobsTrace = 2
            startVersion@(startTrace, startTree) = Example.startVersion alice
            alicesEdits = history alice
            alicesEditsToSyncDirectly = take numEditsToSynchronise alicesEdits
            popVersion3 = foldEditScript (fst <$> alicesEditsToSyncDirectly) startTree
            in
        return Example {
            Example.name = "Motivating Example: Simulating synchronisation of Alice's edits on Stack.pop to Bob's clone",
            Example.colours = StackPopBob.featureColourPalette $ colours alice,
            Example.startVersion = startVersion,
            Example.history = 
                alicesEditsToSyncDirectly++
                [(edit_trace_only $ delta (fst $ alicesEdits !! deleteThatUpdatesBobsTrace) popVersion3, Just $ PNot $ PVariable feature_ImmutableStack)]
        }
