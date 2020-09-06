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
showCode = showCodeAs "" (\_ i -> genIndent i) (\_ s -> s) show

showCodeAs :: (Monoid b) => b -> (Node a -> Int -> b) -> (Node a -> String -> b) -> (Node a -> b) -> AST a -> b
showCodeAs i indentGenerator prtStrWithContext prtNode (Tree n children) = 
  let indentLen = 2
      indent = mappend i $ indentGenerator n indentLen
      prtStr = prtStrWithContext n
      me = prtNode n
      showList sep l = mconcat $ intersperse (prtStr sep) $ (showCodeAs indent indentGenerator prtStrWithContext prtNode) <$> l
      showListNoIndentIncrease sep l = mconcat $ intersperse (prtStr sep) $ showCodeAs i indentGenerator prtStrWithContext prtNode <$> l
      showHead = showCodeAs indent indentGenerator prtStrWithContext prtNode $ head children
  in  
    mconcat $
    case valuetype n of
      ASTT_FuncDef -> [indent, showHead, prtStr " ", me, showList " " $ tail children]
      ASTT_Parameters -> [prtStr "(", showList ", " children, prtStr ")"]
      ASTT_Statements -> [prtStr "\n", i, prtStr "{\n", showListNoIndentIncrease "\n" children, prtStr $ "\n", i, prtStr "}"]
      ASTT_Return -> [indent, prtStr "return ", showList " " children, prtStr ";"]
      ASTT_Condition -> [indent, prtStr "if (", showHead, prtStr ")", showList " " $ tail children]
      ASTT_FuncCall -> [indent, me, showList ", " children, prtStr ";"]
      ASTT_Expression -> return $ if length children == 1 then showHead else error "Expressios can only have one child"
      ASTT_UnaryOp -> return $ if length children == 1 then mappend me showHead else error "Unary operations can only have one child"
      ASTT_BinaryOp -> if length children == 2 then [showHead, prtStr " ", me, prtStr " ", showCodeAs indent indentGenerator prtStrWithContext prtNode $ head $ tail children] else error "Binary operations must have exactly two children"
      ASTT_VarDecl -> return $ showList " " children
      ASTT_VarRef -> return me
      ASTT_Literal -> return me
      ASTT_Type -> return me
      ASTT_File -> [indent, prtStr "FILE [", me, prtStr $ "] {\n", showList "\n" children, prtStr "\n", indent, prtStr "}"]

instance (Show a) => Show (Node a) where
  show n = "("++(show $ uuid n)++", "++(show $ valuetype n)++", "++(show $ value n)++", "++(show $ ntype n)++")"