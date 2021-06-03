{- |
Description: Module for colouring features and feature formulas.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module for colouring features and feature formulas.
We use this colouring for visualizing features when printing to terminal.
-}
module FeatureColour (
    ColourPalette,
    FeatureColourPalette,
    FeatureFormulaColourPalette,
    defaultFeatureFormulaColouring
) where

import Feature
import Propositions
import System.Terminal

-- | A 'ColourPalette' assigns values of type @a@ to 'Color's.
type ColourPalette a m = a -> Color m
-- | A 'FeatureColourPalette' is a 'ColourPalette' for 'Feature's.
type FeatureColourPalette m = ColourPalette Feature m
-- | A 'FeatureFormulaColourPalette' is a 'ColourPalette' for 'FeatureFormula's.
type FeatureFormulaColourPalette m = ColourPalette FeatureFormula m

-- | The default colour to use when no other colour is specified.
defaultColour :: (MonadColorPrinter m) => Color m
defaultColour = white

-- | Inverts the given colour.
-- Currently implemented as @id@ as inverting terminal colours is not possible right away.
negate :: (MonadColorPrinter m) => Color m -> Color m
negate = id

-- | Mixes two colours.
-- As ANSI colours cannot be mixed in a generic way, always returns @bright magenta@.
mix :: (MonadColorPrinter m) => Color m -> Color m -> Color m
mix _ _ = bright magenta -- We cannot mix those ANSI colours so just use magenta to indicate a mixture

-- | Lifts a colour palette over features ('FeatureColourPalette') to a palette over formulas ('FeatureFormulaColourPalette').
defaultFeatureFormulaColouring :: (MonadColorPrinter m) => FeatureColourPalette m -> FeatureFormulaColourPalette m
defaultFeatureFormulaColouring _ Nothing = defaultColour
defaultFeatureFormulaColouring palette (Just p) = defaultNonNullFeatureFormulaColouring palette p

-- | Given a colour palette over features ('FeatureColourPalette'), returns the colour a propositional formula over these features should have.
defaultNonNullFeatureFormulaColouring :: (MonadColorPrinter m) => FeatureColourPalette m -> NonNullFeatureFormula -> Color m
defaultNonNullFeatureFormulaColouring palette (PVariable f) = palette f
defaultNonNullFeatureFormulaColouring palette (PNot p) = FeatureColour.negate $ defaultNonNullFeatureFormulaColouring palette p
defaultNonNullFeatureFormulaColouring _ (PAnd []) = defaultColour
defaultNonNullFeatureFormulaColouring _ (POr  []) = defaultColour
defaultNonNullFeatureFormulaColouring palette (PAnd cs) = foldl1 mix $ defaultNonNullFeatureFormulaColouring palette <$> cs
defaultNonNullFeatureFormulaColouring palette (POr  cs) = foldl1 mix $ defaultNonNullFeatureFormulaColouring palette <$> cs
defaultNonNullFeatureFormulaColouring _ _ = defaultColour