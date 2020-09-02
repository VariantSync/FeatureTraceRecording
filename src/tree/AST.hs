{-# LANGUAGE DeriveTraversable #-}

module AST where

import UUID
import Tree
import Util
import Data.List
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
nodetypeof ASTT_Return = Legator
nodetypeof ASTT_File = Legator
nodetypeof ASTT_VarDecl = Legator
nodetypeof ASTT_FuncCall = Legator
nodetypeof ASTT_Condition = Constituent
nodetypeof ASTT_UnaryOp = Constituent --Plain
nodetypeof ASTT_VarRef = Plain -- leaf type
nodetypeof ASTT_Literal = Plain -- leaf type
nodetypeof ASTT_Type = Plain
nodetypeof ASTT_Expression = Plain -- Virtual
nodetypeof ASTT_Parameters = Plain
nodetypeof ASTT_Statements = Plain
nodetypeof ASTT_BinaryOp = Plain

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

showContent :: Int -> (Node a -> String) -> AST a -> String
showContent i tostr (Tree n children) = 
  let indent = genIndent i
      nextIndent = i + 2
      me = tostr n
      showList sep l = intercalate sep $ showContent nextIndent tostr <$> l
      showListNoIndentIncrease sep l = intercalate sep $ showContent i tostr <$> l
      showHead = showContent nextIndent tostr $ head children
  in  
    case valuetype n of
      ASTT_FuncDef -> indent++showHead++" "++me++(showListNoIndentIncrease " " $ tail children)
      ASTT_Parameters -> "("++(showList ", " children)++")"
      ASTT_Statements -> "\n"++indent++"{\n"++(showList "\n" children)++"\n"++indent++"}"
      ASTT_Return -> indent++"return "++(showList " " children)++";"
      ASTT_Condition -> indent++"if ("++showHead++")"++(showListNoIndentIncrease " " $ tail children)
      ASTT_FuncCall -> indent++me++(showList ", " children)++";"
      ASTT_Expression -> if length children == 1 then showHead else error "Expressios can only have one child"
      ASTT_UnaryOp -> if length children == 1 then me++showHead else error "Unary operations can only have one child"
      ASTT_BinaryOp -> if length children == 2 then showHead++" "++me++" "++(showContent nextIndent tostr $ head $ tail children) else error "Binary operations must have exactly two children"
      ASTT_VarDecl -> (showList " " children)
      ASTT_VarRef -> me
      ASTT_Literal -> me
      ASTT_Type -> me
      ASTT_File -> indent++"FILE ["++me++"] {\n"++(showList "\n" children)++"\n"++indent++"}"

instance (Show a) => Show (Node a) where
  show n = "("++(show $ uuid n)++", "++(show $ valuetype n)++", "++(show $ value n)++", "++(show $ ntype n)++")"