{-# LANGUAGE DeriveTraversable #-}

module AST where

import Tree
import Control.Monad.State

type UUID = Int

data NodeType = Plain | Constituent | Legator deriving (Show, Eq)
data Node a = Node {value::a, ntype::NodeType, uuid::UUID} deriving (Show, Eq, Functor) --, version::Int
type AST a = Tree (Node a)

-- instance (Ord a, Eq a) => Ord (Node a) where
--   v <= w = (value v) <= (value w)

instance (Eq a) => Ord (Node a) where
  v <= w = (uuid v) <= (uuid w)

newNode :: a -> NodeType -> State UUID (Node a)
newNode a t = do num <- get
                 put (num + 1)
                 return Node {value = a, ntype = t, uuid = num} --, version = 0

-- increaseVersion :: Int -> Node a -> Node a
-- increaseVersion x n = Node {version = x + version n, value = value n, ntype = ntype n, uuid = uuid n}

uuidOf :: AST a -> UUID
uuidOf = uuid . element

abstract :: AST a -> AST a
abstract = filterNodes (\(Tree n _) -> ntype n /= Plain)

legatorAncestors :: Eq a => AST a -> AST a -> [AST a]
legatorAncestors root = (filter (\(Tree n _) -> ntype n == Legator)).(ancestors root)
