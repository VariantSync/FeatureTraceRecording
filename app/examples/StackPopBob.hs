module StackPopBob where

import StackPop

import UUID
import Example
import SimpleCXX
import FeatureColour

import System.Terminal ( MonadColorPrinter(yellow) )
import Control.Monad.State (State)

featureColourPalette :: (MonadColorPrinter m) => FeatureColourPalette m
featureColourPalette feature 
    | feature == feature_ImmutableStack = yellow
    | otherwise = StackPop.featureColourPalette feature

-- example :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
-- example = StackPop.example >>= \alice ->
--         let numEdits = 3 in
--         return Example {
--             Example.startTrace = {-take numEdits $-} Example.startTrace alice,
--             Example.startTree = Example.startTree alice,
--             editscript = Example.editscript alice,
--             featurecontexts = Example.featurecontexts alice,
--             colours = StackPopBob.featureColourPalette
--         }
