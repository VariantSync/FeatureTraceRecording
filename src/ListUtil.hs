{- |
Description: A collection of functions that modify lists.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

A collection of functions that modify lists.
-}
module ListUtil where

import Data.List (nub)

-- | Returns the head of the given list as Just or Nothing, iff the list is empty.
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

-- | Returns a continuous sublist within the given range [i, j].
getRange :: Int -> Int -> [a] -> [a]
getRange i j = take (j - i + 1) . drop i

-- | Inserts the given element into the given list at the given index.
-- Subsequent elements will be pushed by one.
insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x = insertListAtIndex i [x]

-- | Inserts all elements from the first list into the second list at the given index.
-- Similar to 'insertAtIndex' but inserts all elements of a list rather than just a single element.
insertListAtIndex :: Int -> [a] -> [a] -> [a]
insertListAtIndex _ l [] = l
insertListAtIndex i arg l@(head:tail)
    | i > 0 = head:insertListAtIndex (i-1) arg tail
    | otherwise = arg++l

-- | Removes the element at given index from the given list.
removeIndex :: Int -> [a] -> [a]
removeIndex i = removeRange i i

-- | Removes all elements within the given range [i, j] from the given list.
removeRange :: Int -> Int -> [a] -> [a]
removeRange _ _ [] = []
removeRange i j (head:tail)
    | i < 0 = error "i < 0 in removeRange"
    | i > j = error "i > j in removeRange"
    | i == 0 && j > 0 = removeRange 0 (j-1) tail
    | i == 0 && j <= 0 = tail
    | otherwise = head:(removeRange (i-1) (j-1) tail)

-- | Removes all duplicates from the given list.
-- If two elements are equal (w.r.t. to @Eq@), the first one will be kept and the second one discarded.
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = nub
