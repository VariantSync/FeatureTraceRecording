module FeatureColour (
    colourOf
) where

import FeatureTrace
import Propositions
import System.Terminal

defaultColour :: (MonadColorPrinter m) => Color m
defaultColour = white

negate :: (MonadColorPrinter m) => Color m -> Color m
negate = id

mix :: (MonadColorPrinter m) => Color m -> Color m -> Color m
mix _ _ = bright magenta -- We cannot mix those ANSI colours so just use magenta to indicate a mixture

colourOf :: (MonadColorPrinter m) => (Feature -> Color m) -> FeatureFormula -> Color m
colourOf _ Nothing = defaultColour
colourOf palette (Just p) = colourOfFormula palette p

colourOfFormula :: (MonadColorPrinter m) => (Feature -> Color m) -> NonNullFeatureFormula -> Color m
colourOfFormula palette (PVariable f) = palette f
colourOfFormula palette (PNot p) = FeatureColour.negate $ colourOfFormula palette p
colourOfFormula palette (PAnd []) = defaultColour
colourOfFormula palette (POr  []) = defaultColour
colourOfFormula palette (PAnd cs) = foldl1 mix $ colourOfFormula palette <$> cs
colourOfFormula palette (POr  cs) = foldl1 mix $ colourOfFormula palette <$> cs
colourOfFormula palette p = defaultColour