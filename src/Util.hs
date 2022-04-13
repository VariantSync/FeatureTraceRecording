{- |
Description: A module containing various utility functions.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

A module containing various utility functions.
-}
module Util where

-- | Generates a string of /i/ spaces where /i/ is the given indent.
genIndent :: Int -> String
genIndent i = replicate i ' '

-- | Folds the given list after reversing it.
reversefoldr :: (a -> b -> b) -> b -> [a] -> b
reversefoldr f zero container = foldr f zero $ reverse container

-- | Generates parenthesis around the given string iff the given bool is true.
parenIf :: Bool -> String -> String
parenIf True s = "("++s++")"
parenIf False s = s

-- | Filters the given Maybe.
-- If the maybe's element satisfies the given predicate, the element will be kept.
-- Otherwise, returns @Nothing@.
takeIf :: (a -> Bool) -> Maybe a -> Maybe a
takeIf _ Nothing = Nothing
takeIf p (Just x)
    | p x = Just x
    | otherwise = Nothing

-- | Lifts a value to a 'Maybe' based on a predicate.
-- Iff the element satisfies the predicate, the result is @Nothing@.
-- Otherwise returns the @Just@ the element.
nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
    | p a = Nothing
    | otherwise = Just a

-- | Removes the first and the last element of a list.
removeFirstAndLast :: [a] -> [a]
removeFirstAndLast [] = []
removeFirstAndLast [x] = []
removeFirstAndLast xs = tail $ init xs

-- | If the given string starts and ends with quotes @"@ (i.e., it is of the form @"\"something\""@), those parenthesis will be removed (i.e., turned to just @"something"@).
-- This is used for showing strings from a polymorphic context.
removeQuotes :: String -> String
removeQuotes s =
    if head s == '\"' && last s == '\"'
    then removeFirstAndLast s
    else s
