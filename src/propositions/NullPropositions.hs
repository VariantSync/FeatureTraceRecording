{- |
Definition and operations on the ternary logic with /null/.
In the paper, we call formulas of this logic /nullable propositional formulas/.
Reuses 'PropositionalFormula's.
-}
module NullPropositions where

import Logic
import Propositions
import Defunctor ( Defunctor(demap) )
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

-- | Simplifies the given formula. Uses 'Propositions.simplify'.
nullable_simplify :: NullableFormula a -> NullableFormula a
nullable_simplify = fmap Propositions.simplify

-- | Pretty Printing for nullable objects such as the nullable propositional logic.
prettyPrint :: (Show a) => Maybe a -> String
prettyPrint Nothing = "null" -- null, none, nothing, empty, unknown
prettyPrint (Just p) = show p

-- 'Logic's over 'Maybe' are againt logics in the sense of the ternary logic by Sobocinski.
-- In particular, 'NullableFormula' is thus a 'Logic'.
instance Logic a => Logic (Maybe a) where
    ltrue = Just ltrue
    lfalse = Just lfalse
    lvalues = Nothing:(Just <$> lvalues)
    
    lnot = fmap lnot
    land l = case catMaybes l of
        [] -> Nothing
        [p] -> Just p
        justs -> Just $ land justs

    leval config m = m >>= Just . leval (demap config)