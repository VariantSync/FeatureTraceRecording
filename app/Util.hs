module Util where

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x [] = [x]
insertAtIndex i x l@(head:tail) = if i <= 0 then x:l else head:(insertAtIndex (i-1) x tail)

genIndent :: Int -> String
genIndent i = concat $ replicate i "  "

crack :: Maybe a -> a
crack Nothing = error "This Maybe does not contain "
crack (Just x) = x