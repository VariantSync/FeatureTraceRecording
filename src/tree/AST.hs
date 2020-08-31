{-# LANGUAGE DeriveTraversable #-}

module AST where

import UUID
import Tree
import Control.Monad.State

-- These are the grammar rules of a language
-- Sigma in the paper.
data ASTTypeAlphabet = 
    ASTT_FuncDef
  | ASTT_Parameters
  | ASTT_Statements
  | ASTT_Return
  | ASTT_Condition
  | ASTT_FuncCall
  | ASTT_Expression
  | ASTT_UnaryOp
  | ASTT_BinaryOp
  | ASTT_VarDecl
  | ASTT_VarRef
  | ASTT_Literal
  | ASTT_Type
  | ASTT_File
  deriving (Eq, Show)

data Node a = Node {value::a, valuetype::ASTTypeAlphabet, uuid::UUID} deriving (Eq, Functor) --, version::Int
type AST a = Tree (Node a)

instance (Eq a) => Ord (Node a) where
  v <= w = (uuid v) <= (uuid w)

{-
Classification for Feature Traces and Presence Condition
There should be a unique mapping ASTTypeAlphabet -> NodeType.
-}
data NodeType = Plain | Constituent | Legator deriving (Show, Eq)
nodetypeof :: ASTTypeAlphabet -> NodeType
nodetypeof ASTT_FuncDef = Legator
nodetypeof ASTT_Parameters = Plain
nodetypeof ASTT_Statements = Plain
nodetypeof ASTT_Return = Legator
nodetypeof ASTT_Condition = Constituent
nodetypeof ASTT_FuncCall = Legator
nodetypeof ASTT_Expression = Constituent -- Expressions only have expressions as children
nodetypeof ASTT_UnaryOp = Constituent
nodetypeof ASTT_BinaryOp = Legator
nodetypeof ASTT_VarDecl = Legator
nodetypeof ASTT_VarRef = Constituent -- leaf type
nodetypeof ASTT_Literal = Constituent -- leaf type
nodetypeof ASTT_Type = Constituent
nodetypeof ASTT_File = Legator

ntype :: Node a -> NodeType
ntype n = nodetypeof $ valuetype n

node :: a -> ASTTypeAlphabet -> State UUID (Node a)
node v vt = do
  next ()
  id <- get
  return Node {value = v, valuetype = vt, uuid = id} --, version = 0

-- increaseVersion :: Int -> Node a -> Node a
-- increaseVersion x n = Node {version = x + version n, value = value n, ntype = ntype n, uuid = uuid n}

uuidOf :: AST a -> UUID
uuidOf = uuid . element

abstract :: AST a -> AST a
abstract = filterNodes (\(Tree n _) -> ntype n /= Plain)

legatorAncestors :: Eq a => AST a -> AST a -> [AST a]
legatorAncestors root = (filter (\(Tree n _) -> ntype n == Legator)).(ancestors root)

instance (Show a) => Show (Node a) where
  show n = "("++(show $ uuid n)++", "++(show $ valuetype n)++", "++(show $ value n)++", "++(show $ ntype n)++")"