module FeatureColour (
    ColourPalette,
    FeatureColourPalette,
    FeatureFormulaColourPalette,
    defaultFeatureFormulaColouring
) where

import FeatureTrace
import Propositions
import System.Terminal

type ColourPalette a m = a -> Color m
type FeatureColourPalette m = ColourPalette Feature m
type FeatureFormulaColourPalette m = FeatureFormula -> Color m

defaultColour :: (MonadColorPrinter m) => Color m
defaultColour = white

negate :: (MonadColorPrinter m) => Color m -> Color m
negate = id

mix :: (MonadColorPrinter m) => Color m -> Color m -> Color m
mix _ _ = bright magenta -- We cannot mix those ANSI colours so just use magenta to indicate a mixture

defaultFeatureFormulaColouring :: (MonadColorPrinter m) => FeatureColourPalette m -> FeatureFormulaColourPalette m
defaultFeatureFormulaColouring _ Nothing = defaultColour
defaultFeatureFormulaColouring palette (Just p) = defaultNonNullFeatureFormulaColouring palette p

defaultNonNullFeatureFormulaColouring :: (MonadColorPrinter m) => FeatureColourPalette m -> NonNullFeatureFormula -> Color m
defaultNonNullFeatureFormulaColouring palette (PVariable f) = palette f
defaultNonNullFeatureFormulaColouring palette (PNot p) = FeatureColour.negate $ defaultNonNullFeatureFormulaColouring palette p
defaultNonNullFeatureFormulaColouring _ (PAnd []) = defaultColour
defaultNonNullFeatureFormulaColouring _ (POr  []) = defaultColour
defaultNonNullFeatureFormulaColouring palette (PAnd cs) = foldl1 mix $ defaultNonNullFeatureFormulaColouring palette <$> cs
defaultNonNullFeatureFormulaColouring palette (POr  cs) = foldl1 mix $ defaultNonNullFeatureFormulaColouring palette <$> cs
defaultNonNullFeatureFormulaColouring _ _ = defaultColour