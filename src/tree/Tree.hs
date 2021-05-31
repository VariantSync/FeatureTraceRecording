{-# LANGUAGE DeriveTraversable #-}

-- | Implementation of a rose tree (i.e., a tree whose nodes can have an arbitrary amount of children, including 0).
module Tree where

import Data.List
import Data.Maybe
import Data.Set

import ListUtil
import Util

-- | Rose tree of elements of type a where children of nodes are given as lists.
data Tree a = Tree a [Tree a] deriving (Eq, Traversable)

instance Show a => Show (Tree a) where
  show = prettyPrint 0 id show

instance Functor Tree where
  fmap f (Tree n c) = Tree (f n) (fmap (fmap f) c)

instance Foldable Tree where
  foldMap f {- a to Monoid -} (Tree x c) = mappend (f x) (mconcat $ fmap (foldMap f) c)

instance Applicative Tree where
   pure a = Tree a []
   (Tree f cf) <*> (Tree x cx) = Tree (f x) ((fmap (fmap f) cx)++(concatMap (\g -> fmap (\c -> g <*> c) cx) cf))

-- | Returns true iff the given tree is a leaf (i.e., it has no children).
isleaf :: Tree a -> Bool
isleaf (Tree _ children) = Data.List.null children

-- | Returns the value stored in the root of the given tree.
element :: Tree a -> a
element (Tree n _) = n

-- | Finds a subtree in the given tree (first argument) whose root has the given value (second argument).
-- Throws an error iff no such tree exists.
tree :: (Eq a, Show a) => Tree a -> a -> Tree a
tree t x = case safetree t x of
  Nothing -> error $ "The element "++(show x)++" is not part of tree "++(show t)
  Just t' -> t'

-- | Same as 'tree' but returns @Nothing@ in the error case (i.e., when no subtree contains the given value).
safetree :: Eq a => Tree a -> a -> Maybe(Tree a)
safetree t x = Tree.find (\(Tree y _) -> x == y) t

-- | Transforms a tree into a set.
-- The returned set contains exactly the values previously held in the tree.
toset :: Ord a => Tree a -> Set a
toset = fromList.(foldMap pure)

-- | Find a subtree in the given tree (first argument) whose root matches the predicate (second argument).
find :: (Tree a -> Bool) -> Tree a -> Maybe(Tree a)
find predicate x@(Tree _ children) = case predicate x of
  True -> Just x
  False -> safehead $ catMaybes $ fmap (\t -> Tree.find predicate t) children

-- | Same as 'find' but takes a predicate over elements instead of a predicate over trees to identify the subtree of interest.
findByNode :: (a -> Bool) -> Tree a -> Maybe(Tree a)
findByNode p = Tree.find (\(Tree n _) -> p n)

-- | Returns the parent of the given node (second argument) in the given tree (first argument).
-- The returned parent is a subtree of the first given tree and has the second given tree as child.
-- Throws an error iff no parent exists.
parent :: Eq a => Tree a -> Tree a -> Maybe(Tree a)
parent root t = Tree.find (\(Tree _ children) -> elem t children) root

{-
| Retrieves all nodes that are above the given node (second argument) in the given tree (first argument)
(This is the transitive closure of 'parent'.)
-}
ancestors :: Eq a => Tree a -> Tree a -> [Tree a]
ancestors root t = case parent root t of
  Nothing -> []
  Just p -> (ancestors root p)++[p]

-- | Applies the given function to each node from bottom to top (i.e., leaves are converted first, root last).
manipulate :: (Tree a -> Tree a) -> Tree a -> Tree a
manipulate f (Tree x children) = f (Tree x (fmap (manipulate f) children))

{-
| Removes all subtrees not meeting the imposed condition.
The root remains untouched.
-}
filterTrees :: (Tree a -> Bool) -> Tree a -> Tree a
filterTrees p = manipulate (\(Tree n c) -> Tree n (Data.List.filter p c))

{-
| Removes all nodes not meeting the imposed condition.
Children of removed nodes are moved up and become children of the parent of the removed node.
The root remains untouched.
-}
filterNodes :: (Tree a -> Bool) -> Tree a -> Tree a
filterNodes p = manipulate (\tree@(Tree node children) ->
    Tree node (concat $ fmap (\c@(Tree _ cc) -> if p c then [c] else cc) children))


-- | Pretty print function that folds the tree into a monoidial type @b@.
-- @b@ is the type that is the output of the print (e.g., String or Text).
-- First argument is the length of the indent in spaces (e.g., 2 will produce an indent of @"  "@ per level).
-- Second argument is a function that lifts strings to the output type b (e.g., a constructor of @b@ accepting a string).
-- Third argument is a function to convert tree elements to the output type (e.g., @show@ if @b@ is @String@).
prettyPrint :: (Show a, Monoid b) => Int -> (String -> b) -> (a -> b) -> Tree a -> b
prettyPrint i strToB nodePrinter (Tree n []) = (strToB $ genIndent i) <> (nodePrinter n) <>  (strToB " []\n")
prettyPrint i strToB nodePrinter (Tree n children) = (strToB $ genIndent i)
  <> (nodePrinter n)
  <> (strToB " [\n")
  <> (mconcat $ fmap (prettyPrint (i+2) strToB nodePrinter) children)
  <> (strToB $ genIndent i)
  <> (strToB "]\n")