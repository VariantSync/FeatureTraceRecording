{- |
Description: Motivating Example: Simulating propagation of Alice's edits on Stack.pop to Bob's clone
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module for reproducing Bob's part of our motivating 'example'.
Bob propagates applicable edits by Alice to his variant.
The example is described in detail in Section 2.2 of the paper and shown in Figure 3.
-}
module StackPopBob where

import StackPopAlice ( feature_ImmutableStack, example )

import UUID ( UUID )
import Example ( Example(..) )
import Edits ( Edit(delta), foldEditScript, edit_trace_only )
import Propositions ( PropositionalFormula(..) )
import SimpleJava ( SimpleJavaGrammar )
import FeatureColour

import System.Terminal
import Control.Monad.State ( State ) 

{- |
Colours for features and feature formulas used in this example.
We chose terminal colours as close the the colours used in the paper as possible.
-}
featureColourPalette :: (MonadColorPrinter m) => FeatureFormulaColourPalette m -> FeatureFormulaColourPalette m
featureColourPalette fallback formula 
    | formula == (Just $ PVariable $ feature_ImmutableStack) = magenta
    | otherwise = fallback formula

{- |
Example replaying our Bob's part of our motivating example shown in Figure 3 and described in Section 2.2 in our paper.
Bob propagates Alice's changes on the @pop@ method to his variant.
This example directly reuses the first two edits of Alice and appends an artifical noop edit ('edit_trace_only') to update the feature mappings
that were recorded upon Alice's edits that were not applicable to Bob's variant but induced further feature mappings.
-}
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
            Example.name = "Motivating Example: Simulating propagation of Alice's edits on Stack.pop to Bob's clone",
            Example.colours = StackPopBob.featureColourPalette $ colours alice,
            Example.startVersion = startVersion,
            Example.history = 
                alicesEditsToSyncDirectly++
                [(edit_trace_only $ delta (fst $ alicesEdits !! deleteThatUpdatesBobsTrace) popVersion3, Just $ PNot $ PVariable feature_ImmutableStack)]
        }
