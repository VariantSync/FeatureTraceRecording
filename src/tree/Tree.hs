{-# LANGUAGE DeriveTraversable #-}

module Tree where

import Data.List
import Data.Maybe
import Data.Set

import ListUtil
import Util

data Tree a = Tree a [Tree a] deriving (Eq, Traversable)

prettyPrint :: (Show a, Monoid b) => Int -> (String -> b) -> (a -> b) -> Tree a -> b
prettyPrint i strToB nodePrinter (Tree n []) = (strToB $ genIndent i) <> (nodePrinter n) <>  (strToB " []\n")
prettyPrint i strToB nodePrinter (Tree n children) = (strToB $ genIndent i)
  <> (nodePrinter n)
  <> (strToB " [\n")
  <> (mconcat $ fmap (prettyPrint (i+2) strToB nodePrinter) children)
  <> (strToB $ genIndent i)
  <> (strToB "]\n")

instance Show a => Show (Tree a) where
  show = prettyPrint 0 id show

instance Functor Tree where
  fmap f (Tree n c) = Tree (f n) (fmap (fmap f) c)

instance Foldable Tree where
  foldMap f {- a to Monoid -} (Tree x c) = mappend (f x) (mconcat $ fmap (foldMap f) c)

instance Applicative Tree where
   pure a = Tree a []
   (Tree f cf) <*> (Tree x cx) = Tree (f x) ((fmap (fmap f) cx)++(concatMap (\g -> fmap (\c -> g <*> c) cx) cf))

-- instance Monad Tree where
--   return = pure
--   (Tree x c) >>= f = f x


--instance Traversable Tree where
--  traverse g (Tree x []) = fmap pure (g x)
--  traverse g t@(Tree x c) = fmap (traverse g) c
  
isleaf :: Tree a -> Bool
isleaf (Tree _ children) = Data.List.null children

element :: Tree a -> a
element (Tree n _) = n

tree :: (Eq a, Show a) => Tree a -> a -> Tree a
tree t x = case safetree t x of
  Nothing -> error $ "The element "++(show x)++" is not part of tree "++(show t)
  Just t' -> t'

safetree :: Eq a => Tree a -> a -> Maybe(Tree a)
safetree t x = Tree.find (\(Tree y _) -> x == y) t

toset :: Ord a => Tree a -> Set a
toset = fromList.(foldMap pure)

{-
Find the first subtree in the given tree (first argument) whose root matches the predicate (second argument).
-}
find :: (Tree a -> Bool) -> Tree a -> Maybe(Tree a)
find predicate x@(Tree _ children) = case predicate x of
  True -> Just x
  False -> safehead $ catMaybes $ fmap (\t -> Tree.find predicate t) children

findWithNode :: (a -> Bool) -> Tree a -> Maybe(Tree a)
findWithNode p = Tree.find (\(Tree n _) -> p n)

parent :: Eq a => Tree a -> Tree a -> Maybe(Tree a)
parent root t = Tree.find (\(Tree _ children) -> elem t children) root

{-
Retrieves all nodes that are above the given node (second argument) in the given tree (first argument)
-}
ancestors :: Eq a => Tree a -> Tree a -> [Tree a]
ancestors root t = case parent root t of
  Nothing -> []
  Just p -> (ancestors root p)++[p]

manipulate :: (Tree a -> Tree a) -> Tree a -> Tree a
manipulate f (Tree x children) = f (Tree x (fmap (manipulate f) children))

{-
Removes all subtrees not meeting the imposed condition.
The root remains untouched.
-}
filterTrees :: (Tree a -> Bool) -> Tree a -> Tree a
filterTrees p = manipulate (\(Tree n c) -> Tree n (Data.List.filter p c))

{-
Removes all nodes not meeting the imposed condition.
Children of removed nodes are moved up and become children of the parent of the removed node.
The root remains untouched.
-}
filterNodes :: (Tree a -> Bool) -> Tree a -> Tree a
filterNodes p = manipulate (\tree@(Tree node children) ->
    Tree node (concat $ fmap (\c@(Tree _ cc) -> if p c then [c] else cc) children))