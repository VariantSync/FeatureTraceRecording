{-# LANGUAGE DeriveTraversable #-}

module FTRNode where

import AST
import Control.Monad.State

type UUID = Int

data NodeType = Plain | Constituent | Legator deriving (Show, Eq)
data Node a = Node {value::a, ntype::NodeType, uuid::UUID} deriving (Show, Functor, Eq, Foldable, Traversable)
-- data FTRAST = AST (FTRNode a)

newNode :: a -> NodeType -> State UUID (Node a)
newNode a t = do num <- get
                 put (num + 1)
                 return Node {value = a, ntype = t, uuid = num}

uuidOf :: AST (Node a) -> UUID
uuidOf = uuid . element