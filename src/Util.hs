module Util where

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x [] = [x]
insertAtIndex i x l@(head:tail) = if i <= 0 then x:l else head:(insertAtIndex (i-1) x tail)

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