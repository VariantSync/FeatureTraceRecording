﻿module StackPopBob where

import StackPopAlice ( feature_ImmutableStack, example )

import UUID ( UUID )
import Example ( Example(..) )
import Edits ( Edit(delta), foldEditScript, edit_trace_only )
import Propositions ( PropositionalFormula(..) )
import SimpleCXX ( SimpleCXXGrammar )
import FeatureColour
import FeatureTrace

import System.Terminal
import Control.Monad.State ( State ) 

featureColourPalette :: (MonadColorPrinter m) => FeatureFormulaColourPalette m -> FeatureFormulaColourPalette m
featureColourPalette fallback formula 
    | formula == (Just $ PVariable $ feature_ImmutableStack) = magenta
    | otherwise = fallback formula

example :: MonadColorPrinter m => State UUID (Example m SimpleCXXGrammar String)
example =
    StackPopAlice.example
    >>= \alice ->
        let numEditsToSynchronise = 2
            deleteThatUpdatesBobsTrace = 2
            startTree = Example.startTree alice
            alicesEdits = editscript alice
            alicesEditsToSyncDirectly = take numEditsToSynchronise alicesEdits
            popVersion3 = foldEditScript alicesEditsToSyncDirectly startTree
            in
        return Example {
            Example.name = "Motivating Example: Simulating synchronisation of Alice's edits on Stack.pop to Bob's clone",
            Example.colours = StackPopBob.featureColourPalette $ colours alice,
            Example.startTrace = Example.startTrace alice,
            Example.startTree = startTree,
            Example.editscript = 
                alicesEditsToSyncDirectly++
                [edit_trace_only $ delta (alicesEdits !! deleteThatUpdatesBobsTrace) popVersion3],
            Example.featurecontexts =
                (take numEditsToSynchronise $ Example.featurecontexts alice)++
                [Just $ PNot $ PVariable feature_ImmutableStack]
        }