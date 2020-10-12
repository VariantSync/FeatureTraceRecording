module AST where

import UUID
import Tree
import Util
import Control.Monad.State

{-
Classification for Feature Traces and Presence Condition
There should be a unique mapping ASTTypeAlphabet -> NodeType.
-}
data NodeType = Mandatory | Optional | Treeoptional deriving (Show, Eq)

class Show g => Grammar g where
  nodetypeof :: g -> NodeType
  prettyPrint :: (Monoid b) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b

-- g has to be a grammar
data Node g a = Node {value::a, rule::g, uuid::UUID} --, version::Int
type AST g a = Tree (Node g a)

instance Eq (Node g a) where
  n == m = (uuid n) == (uuid m)

instance (Eq a) => Ord (Node g a) where
  v <= w = (uuid v) <= (uuid w)

instance Functor (Node g) where
  fmap f n = Node {value = f $ value n, rule = rule n, uuid = uuid n}

ntype :: Grammar g => Node g a -> NodeType
ntype = nodetypeof.rule

node :: Grammar g => a -> g -> State UUID (Node g a)
node v vt = do
  next ()
  id <- get
  return Node {value = v, rule = vt, uuid = id} --, version = 0

uuidOf :: AST g a -> UUID
uuidOf = uuid . element

findById :: UUID -> AST g a -> Maybe (AST g a)
findById i = Tree.find ((i==).uuidOf)

findByValue :: (Eq a) => a -> AST g a -> Maybe (AST g a)
findByValue v = findByNode (\n -> value n == v)

findByRule :: (Eq g) => g -> AST g a -> Maybe (AST g a)
findByRule r = findByNode ((r==).rule)

abstract :: Grammar g => AST g a -> AST g a
abstract = filterNodes (\(Tree n _) -> ntype n /= Mandatory)

treeoptionalAncestors :: (Eq a, Grammar g) => AST g a -> AST g a -> [AST g a]
treeoptionalAncestors root = (filter (\(Tree n _) -> ntype n == Treeoptional)).(ancestors root)

showCode :: (Show a, Grammar g) => AST g a -> String
showCode = showCodeAs "" (\_ i -> genIndent i) (\_ s -> s) show

showCodeAs :: (Monoid b, Grammar g) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b
showCodeAs = AST.prettyPrint

instance (Grammar g, Show a) => Show (Node g a) where
  show n = "("++(show $ uuid n)++", "++(show $ rule n)++", "++(show $ value n)++", "++(show $ ntype n)++")"