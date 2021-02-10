{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AST where

import UUID
import Tree
import Grammar
import Control.Monad.State
import StructuralElement

-- g has to be a grammar
data Node g a = Node {value::a, grammartype::g, uuid::UUID} --, version::Int
type AST g a = Tree (Node g a)

instance Eq (Node g a) where
  n == m = (uuid n) == (uuid m)

instance Ord (Node g a) where
  v <= w = (uuid v) <= (uuid w)

instance Functor (Node g) where
  fmap f n = Node {value = f $ value n, grammartype = grammartype n, uuid = uuid n}

instance (Grammar g, Eq a, Show a) => StructuralElement (AST g a) where
  se_canBeAnnotated (Tree n _) = optionaltype n /= Mandatory
  se_isWrapper (Tree n _) = optionaltype n == Optional
  se_parent = parent
  se_children (Tree _ c) = c

optionaltype :: Grammar g => Node g a -> NodeType
optionaltype = nodetypeof.grammartype

node :: Grammar g => a -> g -> State UUID (Node g a)
node v vt = do
  next ()
  id <- get
  return Node {value = v, grammartype = vt, uuid = id} --, version = 0

uuidOf :: AST g a -> UUID
uuidOf = uuid . element

findById :: UUID -> AST g a -> Maybe (AST g a)
findById i = Tree.find ((i==).uuidOf)

findByValue :: (Eq a) => a -> AST g a -> Maybe (AST g a)
findByValue v = findByNode (\n -> value n == v)

findByGrammarType :: (Eq g) => g -> AST g a -> Maybe (AST g a)
findByGrammarType r = findByNode ((r==).grammartype)

abstract :: Grammar g => AST g a -> AST g a
abstract = filterNodes (\(Tree n _) -> optionaltype n /= Mandatory)

treeoptionalAncestors :: (Eq a, Grammar g) => AST g a -> AST g a -> [AST g a]
treeoptionalAncestors root = (filter (\(Tree n _) -> optionaltype n == Treeoptional)).(ancestors root)

instance (Grammar g, Show a) => Show (Node g a) where
  show n = "("++(show $ uuid n)++", "++(show $ grammartype n)++", "++(show $ value n)++", "++(show $ optionaltype n)++")"