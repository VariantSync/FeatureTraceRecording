module StructuralElement where

class (Ord s, Show s) => StructuralElement s where
    se_canBeAnnotated :: s -> Bool
    se_isWrapper :: s -> Bool
    -- root -> subtree -> parent of subtree in root
    se_parent :: s -> s -> Maybe s
    se_children :: s -> [s]

se_ancestors :: (StructuralElement s) => s -> s -> [s]
se_ancestors root x = case se_parent root x of
  Nothing -> []
  Just p -> (se_ancestors root p)++[p]

se_propagatingAncestors :: (StructuralElement s) => s -> s -> [s]
se_propagatingAncestors root x = filter (\v -> (se_canBeAnnotated v) && (not $ se_isWrapper v)) $ se_ancestors root x

-- pc parent part
se_propagate :: (StructuralElement s) => s -> (s -> a) -> ([a] -> a) -> s -> Maybe a
se_propagate root getValue simpleFold x
    | se_canBeAnnotated x = case se_propagatingAncestors root x of
        [] -> Nothing
        l@(y:ys) -> Just $ simpleFold (getValue <$> l)
    | otherwise = se_parent root x >>= se_propagate root getValue simpleFold