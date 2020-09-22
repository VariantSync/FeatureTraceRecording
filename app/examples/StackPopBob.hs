module StackPopBob where

import StackPopAlice ( feature_ImmutableStack, example )

import UUID ( UUID )
import Example
import SimpleCXX ( SimpleCXXGrammar )
import FeatureColour ( FeatureColourPalette )

import System.Terminal ( MonadColorPrinter(yellow) )
import Control.Monad.State ( State ) 

featureColourPalette :: (MonadColorPrinter m) => FeatureColourPalette m -> FeatureColourPalette m
featureColourPalette fallback feature 
    | feature == feature_ImmutableStack = yellow
    | otherwise = fallback feature

example :: MonadColorPrinter m => State UUID (Example m SimpleCXXGrammar String)
example =
    StackPopAlice.example
    >>= \alice ->
        let numEditsToSynchronise = 2 in
        return Example {
            name = "Simulating synchronisation of Alice's edits on Stack.pop to Bob's clone",
            colours = StackPopBob.featureColourPalette $ colours alice,
            startTrace = Example.startTrace alice,
            startTree = Example.startTree alice,
            editscript = 
                (take numEditsToSynchronise $ Example.editscript alice),
            featurecontexts =
                (take numEditsToSynchronise $ Example.featurecontexts alice)
        }
