{-# LANGUAGE TypeFamilies #-}

{- |
Description: Definition and operations on the ternary logic with /null/.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Definition and operations on the ternary logic with /null/.
In the paper, we call formulas of this logic /nullable propositional formulas/.
Reuses 'PropositionalFormula's.
-}
module Propositions.NullPropositions where

import Propositions.Logic
import Propositions.Propositions
import Data.Maybe (catMaybes, fromJust, isNothing)

-- | Data type for the ternary logic by Sobocinski.
--  The 'Nothing' case represents /null/ as used in our paper.
type NullableFormula a = Maybe (PropositionalFormula a)

-- | Returns @true@ iff the given formula is the value /null/.
isnull :: NullableFormula a -> Bool
isnull = isNothing

-- | Returns @false@ iff the given formula is the value /null/.
notnull :: NullableFormula a -> Bool
notnull = not.isnull

-- | Converts the given nullable formula to a propositional formula, assuming that the given formula is not /null/.
-- Crashes otherwise.
assure :: NullableFormula a -> PropositionalFormula a
assure = fromJust

-- | Simplifies the given formula. Uses 'Propositions.Propositions.simplify'.
nullable_simplify :: NullableFormula a -> NullableFormula a
nullable_simplify = fmap simplify

-- | Pretty Printing for nullable objects such as the nullable propositional logic.
prettyPrint :: (Show a) => Maybe a -> String
prettyPrint Nothing = "null" -- null, none, nothing, empty, unknown
prettyPrint (Just p) = show p

-- | Any 'Logic' can be lifted to a logic on 'Maybe'.
-- This adds a new value 'Nothing' to the values of the given logic.
-- In particular, 'NullableFormula' is thus a 'Logic'.
instance Logic a => Logic (Maybe a) where
    type Value (Maybe a) = Maybe (Value a)
    type Variable (Maybe a) = Variable a
    type VariableValue (Maybe a) = VariableValue a

    ltrue = Just ltrue
    lfalse = Just lfalse
    lvalues = Nothing:(Just <$> lvalues)
    
    lnot = fmap lnot
    land l = case catMaybes l of
        [] -> Nothing
        [p] -> Just p
        justs -> Just $ land justs

    leval config m = leval config <$> m
