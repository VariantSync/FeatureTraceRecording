{- |
Description: Data types and operations for Abstract Syntax Trees ('AST's).
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Data types and operations for Abstract Syntax Trees (ASTs).
'AST's are that 'Tree's with a fixed 'Node' type.
-}
module AST where

import UUID
import Tree
import Grammar
import Control.Monad.State

-- | Node type of 'AST's.
data Node g a = Node {
  -- | The value that is encapsulated by a node. Most of the time this is a @String@ referring to source code statements.
  value::a,
  -- | The grammar rule this node was parsed from. @g@ should be an instance of 'Grammar'.
  grammartype::g,
  -- | A unique identifier to trace nodes across several versions of 'AST's.
  uuid::UUID
}

instance Eq (Node g a) where
  n == m = (uuid n) == (uuid m)

instance (Eq a) => Ord (Node g a) where
  v <= w = (uuid v) <= (uuid w)

instance Functor (Node g) where
  fmap f n = n {value = f $ value n}

instance (Grammar g, Show a) => Show (Node g a) where
  show n = "("++(show $ uuid n)++", "++(show $ grammartype n)++", "++(show $ value n)++", "++(show $ optionaltype n)++")"

-- | Type for abstract syntax trees ('AST's) representing source code.
-- It is a tree in which each node represents a source code entity by a value, a grammar type and a 'UUID'.
type AST g a = Tree (Node g a)

-- | Returns the node type of a nodes grammar type.
optionaltype :: Grammar g => Node g a -> NodeType
optionaltype = nodetypeof.grammartype

-- | Creates a new node from a value and a type by generating a new 'UUID' for it.
node :: Grammar g => a -> g -> State UUID (Node g a)
node v vt = do
  id <- next
  return Node {value = v, grammartype = vt, uuid = id}

-- | Returns the 'UUID' of an 'AST's root.
uuidOf :: AST g a -> UUID
uuidOf = uuid . element

-- | Finds a subtree in the given 'AST' whose root has the given 'UUID'. Returns @Nothing@ iff no such subtree exists.
findById :: UUID -> AST g a -> Maybe (AST g a)
findById i = Tree.find ((i==).uuidOf)

-- | Finds a subtree in the given 'AST' whose root has the given value. Returns @Nothing@ iff no such subtree exists.
findByValue :: (Eq a) => a -> AST g a -> Maybe (AST g a)
findByValue v = findByNode (\n -> value n == v)

-- | Finds a subtree in the given 'AST' whose root has the given grammar type. Returns @Nothing@ iff no such subtree exists.
findByGrammarType :: (Eq g) => g -> AST g a -> Maybe (AST g a)
findByGrammarType r = findByNode ((r==).grammartype)

-- | Returns all ancestors of a given subtree (second argument) in the given tree (first argument), that are optional (i.e., their 'optionaltype' is @Optional@).
optionalAncestors :: (Eq a, Grammar g) => AST g a -> AST g a -> [AST g a]
optionalAncestors root = (filter (\(Tree n _) -> optionaltype n == Optional)).(ancestors root)