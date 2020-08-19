module Util where

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x [] = [x]
insertAtIndex i x l@(head:tail) = if i <= 0 then x:l else head:(insertAtIndex (i-1) x tail)

genIndent :: Int -> String
genIndent i = concat $ replicate i "  "

hasvalue :: Maybe a -> Bool
hasvalue Nothing = False
hasvalue (Just x) = True

crack :: Maybe a -> a
crack Nothing = error "This Maybe does not contain a value"
crack (Just x) = x

safecrack :: Maybe a -> (() -> a) -> a
safecrack Nothing gen = gen ()
safecrack (Just x) gen = x

reversefoldr :: (a -> b -> b) -> b -> [a] -> b
reversefoldr f zero container = foldr f zero $ reverse container

parenIf :: Bool -> String -> String
parenIf True s = "("++s++")"
parenIf False s = s