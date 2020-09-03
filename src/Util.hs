module Util where

genIndent :: Int -> String
genIndent i = concat $ replicate i "  "

safeFromJust :: Maybe a -> (() -> a) -> a
safeFromJust Nothing gen = gen ()
safeFromJust (Just x) _ = x

reversefoldr :: (a -> b -> b) -> b -> [a] -> b
reversefoldr f zero container = foldr f zero $ reverse container

parenIf :: Bool -> String -> String
parenIf True s = "("++s++")"
parenIf False s = s

takeIf :: (a -> Bool) -> Maybe a -> Maybe a
takeIf _ Nothing = Nothing
takeIf p (Just x)
    | p x = Just x
    | otherwise = Nothing

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
    | p a = Nothing
    | otherwise = Just a