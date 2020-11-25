module NullPropositions where

import Logic
import Propositions
import Defunctor ( Defunctor(demap) )
import Data.Maybe (catMaybes, fromJust, isNothing)

-- The Nothing case represents null
type NullableFormula a = Maybe (PropositionalFormula a)
-- data NullableFormula a = 
--       PNull
--     | PTrue
--     | PFalse
--     | PVariable a
--     | PNot (PropositionalFormula a)
--     | PAnd [PropositionalFormula a]
--     | POr [PropositionalFormula a]
--     deriving (Eq)

isnull :: NullableFormula a -> Bool
isnull = isNothing

notnull :: NullableFormula a -> Bool
notnull = not.isnull

assure :: NullableFormula a -> PropositionalFormula a
assure = fromJust

nullable_simplify :: NullableFormula a -> NullableFormula a
nullable_simplify = fmap Propositions.simplify

prettyPrint :: (Show a) => Maybe a -> String
prettyPrint Nothing = "null" -- null, none, nothing, empty, unknown
prettyPrint (Just p) = show p

-- instance Logic a b => Logic a (Maybe b) where
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