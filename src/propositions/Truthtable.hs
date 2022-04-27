{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Description: Generation of truth tables for 'Logic's.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Generation of truth tables for 'Logic's.
-}
module Truthtable where

import Logic
import Data.Void

-- | For a unary operator and a set of values, produces all results when applying that operator to that set of values.
unarytable :: (Logic a, Variable a ~ Void) => (a -> a) -> [a] -> [Value a]
unarytable op values = leval absurd . op <$> values

-- | Builds the cartesions product of two lists.
cartesian :: [a] -> [b] -> [(a, b)]
cartesian x y = (,) <$> x <*> y

-- | For a binary operator and a set of values, produces all results when applying that operator to all combinations of values.
binarytable :: (Logic a, Variable a ~ Void) => (a -> a -> a) -> [a] -> [Value a]
binarytable op values = leval absurd . uncurry op <$> cartesian values values

-- | Creates a function of two arguments by reusing a function on lists.
binarify :: ([a] -> b) -> (a -> a -> b)
binarify f = \x -> \y -> f [x, y]

-- | Appends spaces to the given string until it has length @i@ (first argument).
hfillto :: Int -> String -> String
hfillto i s
    | missingspace > 0 = (s++) $ replicate missingspace ' '
    | otherwise = s
    where missingspace = i - length s

-- | Default value for width of lines when printing truth tables.
defaultlinewidth :: Int
defaultlinewidth = 7

-- | 'hfillto' with 'defaultlinewidth'
defaulthfillto :: String -> String
defaulthfillto = hfillto defaultlinewidth

-- | For a list of values and a unary operator, generates a truth table with the given name.
prettyunarytable :: (Logic a, Variable a ~ Void, Show a, Show (Value a)) => [a] -> (a -> a) -> String -> String
prettyunarytable values op name = ((name++"\n"++(replicate (2*defaultlinewidth+3) '_')++"\n")++) $ mconcat $ (\(val, res) -> unwords [defaulthfillto $ show val, "|", defaulthfillto $ show res, "\n"]) <$> zip values (unarytable op values)

-- | For a list of values and a binary operator, generates a truth table with the given name.
prettybinarytable :: (Logic a, Variable a ~ Void, Show a, Show (Value a)) => [a] -> (a -> a -> a) -> String -> String
prettybinarytable values op name = ((name++"\n"++(replicate (3*defaultlinewidth+4) '_')++"\n")++) $ mconcat $ (\((a, b), res) -> unwords [defaulthfillto $ show a, defaulthfillto $ show b, "|", defaulthfillto $ show res, "\n"]) <$> zip pairs (binarytable op values)
    where pairs = cartesian values values

-- | Generates a truthtable for all common operators of a logic (not, and, or, implies, equals) for the given set of values.
generatetruthtablesfor :: (Logic a, Variable a ~ Void, Show a, Show (Value a)) => [a] -> String
generatetruthtablesfor values =
    let
        notname = "¬"
        andname = "∧"
        orname  = "∨"
        impliesname = "⇒"
        equalsname = "⇔"
        in
    prettyunarytable values lnot notname
    ++ mconcat (("\n\n"++).uncurry (prettybinarytable values)
        <$> [
              (binarify land, andname)
            , (binarify lor, orname)
            , (limplies, impliesname)
            , (lequals, equalsname)
            ])