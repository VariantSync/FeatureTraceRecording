module SimpleCXX where

import Tree
import AST
import Data.List

type SCXXAST a = AST SimpleCXXGrammar a

data SimpleCXXGrammar = 
    SCXX_FuncDef
  | SCXX_Parameters
  | SCXX_Statements
  | SCXX_ExprStatement
  | SCXX_Return
  | SCXX_Condition
  | SCXX_FuncCall
  | SCXX_Expression
  | SCXX_UnaryOp
  | SCXX_BinaryOp
  | SCXX_VarDecl
  | SCXX_VarRef
  | SCXX_Literal
  | SCXX_Type
  | SCXX_File
  deriving (Eq, Show)

instance Grammar SimpleCXXGrammar where
    nodetypeof SCXX_FuncDef = Legator
    nodetypeof SCXX_Return = Legator
    nodetypeof SCXX_File = Legator
    nodetypeof SCXX_ExprStatement = Legator
    nodetypeof SCXX_VarDecl = Legator
    nodetypeof SCXX_Condition = Constituent
    nodetypeof SCXX_UnaryOp = Constituent --Plain
    nodetypeof SCXX_FuncCall = Plain
    nodetypeof SCXX_VarRef = Plain -- leaf type
    nodetypeof SCXX_Literal = Plain -- leaf type
    nodetypeof SCXX_Type = Plain
    nodetypeof SCXX_Expression = Plain -- Virtual
    nodetypeof SCXX_Parameters = Plain
    nodetypeof SCXX_Statements = Plain
    nodetypeof SCXX_BinaryOp = Plain
    prettyPrint i indentGenerator prtStrWithContext prtNode (Tree n children) =
        mconcat $
        let indentLen = 2
            indent = mappend i $ indentGenerator n indentLen
            prtStr = prtStrWithContext n
            me = prtNode n
            showList sep l = mconcat $ intersperse (prtStr sep) $ (showCodeAs indent indentGenerator prtStrWithContext prtNode) <$> l
            showListNoIndentIncrease sep l = mconcat $ intersperse (prtStr sep) $ showCodeAs i indentGenerator prtStrWithContext prtNode <$> l
            showHead = showCodeAs indent indentGenerator prtStrWithContext prtNode $ head children
            in case rule n of
                SCXX_FuncDef -> [indent, showHead, prtStr " ", me, showList " " $ tail children]
                SCXX_Parameters -> [prtStr "(", showList ", " children, prtStr ")"]
                SCXX_Statements -> [
                    prtStr "\n",
                    i, prtStr "{\n",
                    if null children
                    then mempty
                    else (mappend (showListNoIndentIncrease "\n" children) (prtStr "\n")),
                    i, prtStr "}"]
                SCXX_ExprStatement -> [indent, showList " " children, prtStr ";"]
                SCXX_Return -> [indent, prtStr "return ", showList " " children, prtStr ";"]
                SCXX_Condition -> [indent, prtStr "if (", showHead, prtStr ")", showList " " $ tail children]
                SCXX_FuncCall -> [me, showList ", " children]
                SCXX_Expression -> return $ if length children == 1 then showHead else error "Expressios can only have one child"
                SCXX_UnaryOp -> return $ if length children == 1 then mappend me showHead else error "Unary operations can only have one child"
                SCXX_BinaryOp -> if length children == 2 then [showHead, prtStr " ", me, prtStr " ", showCodeAs indent indentGenerator prtStrWithContext prtNode $ head $ tail children] else error "Binary operations must have exactly two children"
                SCXX_VarDecl -> return $ showList " " children
                SCXX_VarRef -> return me
                SCXX_Literal -> return me
                SCXX_Type -> return me
                SCXX_File -> [indent, prtStr "FILE [", me, prtStr $ "] {\n", showList "\n" children, prtStr "\n", indent, prtStr "}"]
