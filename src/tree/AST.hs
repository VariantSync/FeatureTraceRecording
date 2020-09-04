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

data Node a = Node {value::a, valuetype::ASTTypeAlphabet, uuid::UUID} --, version::Int
type AST a = Tree (Node a)

instance Eq (Node a) where
  n == m = (uuid n) == (uuid m)

instance (Eq a) => Ord (Node a) where
  v <= w = (uuid v) <= (uuid w)

instance Functor Node where
  fmap f n = Node {value = f $ value n, valuetype = valuetype n, uuid = uuid n}

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

showCode :: (Show a) => AST a -> String
showCode = showCodeAs 0 (\_ s -> s) show

showCodeAs :: (Monoid b) => Int -> (Node a -> String -> b) -> (Node a -> b) -> AST a -> b
showCodeAs i prtStrWithContext prtNode (Tree n children) = 
  let indent = genIndent i
      nextIndent = i + 2
      prtStr = prtStrWithContext n
      me = prtNode n
      showList sep l = mconcat $ intersperse (prtStr sep) $ (showCodeAs nextIndent prtStrWithContext prtNode) <$> l
      showListNoIndentIncrease sep l = mconcat $ intersperse (prtStr sep) $ showCodeAs i prtStrWithContext prtNode <$> l
      showHead = showCodeAs nextIndent prtStrWithContext prtNode $ head children
  in  
    mconcat $
    case valuetype n of
      ASTT_FuncDef -> [prtStr indent, showHead, prtStr " ", me, showListNoIndentIncrease " " $ tail children]
      ASTT_Parameters -> [prtStr "(", showList ", " children, prtStr ")"]
      ASTT_Statements -> [prtStr $ "\n"++indent++"{\n", showList "\n" children, prtStr $ "\n"++indent++"}"]
      ASTT_Return -> [prtStr $ indent++"return ", showList " " children, prtStr ";"]
      ASTT_Condition -> [prtStr $ indent++"if (", showHead, prtStr ")", showListNoIndentIncrease " " $ tail children]
      ASTT_FuncCall -> [prtStr indent, me, showList ", " children, prtStr ";"]
      ASTT_Expression -> return $ if length children == 1 then showHead else error "Expressios can only have one child"
      ASTT_UnaryOp -> return $ if length children == 1 then mappend me showHead else error "Unary operations can only have one child"
      ASTT_BinaryOp -> if length children == 2 then [showHead, prtStr " ", me, prtStr " ", showCodeAs nextIndent prtStrWithContext prtNode $ head $ tail children] else error "Binary operations must have exactly two children"
      ASTT_VarDecl -> return $ showList " " children
      ASTT_VarRef -> return me
      ASTT_Literal -> return me
      ASTT_Type -> return me
      ASTT_File -> [prtStr $ indent++"FILE [", me, prtStr $ "] {\n", showList "\n" children, prtStr $ "\n"++indent++"}"]

instance (Show a) => Show (Node a) where
  show n = "("++(show $ uuid n)++", "++(show $ valuetype n)++", "++(show $ value n)++", "++(show $ ntype n)++")"