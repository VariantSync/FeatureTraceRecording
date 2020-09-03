module ListUtil where

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

getRange :: Int -> Int -> [a] -> [a]
getRange i j [] = []
getRange i j l = (l!!) <$> [i..j]

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex i x [] = [x]
insertAtIndex i x l@(head:tail)
    | i <= 0 = x:l 
    | otherwise = head:(insertAtIndex (i-1) x tail)

insertListAtIndex :: Int -> [a] -> [a] -> [a]
insertListAtIndex i l [] = l
insertListAtIndex i arg l@(head:tail)
    | i > 0 = head:insertListAtIndex (i-1) arg tail
    | otherwise = arg++l

removeIndex :: Int -> [a] -> [a]
removeIndex i = removeRange i i

removeRange :: Int -> Int -> [a] -> [a]
removeRange i j [] = []
removeRange i j (head:tail)
    | i < 0 = error "i < 0 in removeRange"
    | i > j = error "i > j in removeRange"
    | i == 0 && j > 0 = removeRange 0 (j-1) tail
    | i == 0 && j <= 0 = tail
    | otherwise = head:(removeRange (i-1) (j-1) tail)