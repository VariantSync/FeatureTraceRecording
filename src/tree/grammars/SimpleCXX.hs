module SimpleCXX where

import Tree
import Control.Monad.State ( State )
import UUID ( UUID )
import AST
import Data.List ( intersperse )

type SCXXAST a = AST SimpleCXXGrammar a
type SSCXXAST = SCXXAST String

data SimpleCXXGrammar = 
    SCXX_FuncDef
  | SCXX_ParametersDef
  | SCXX_Args
  | SCXX_Statements
  | SCXX_ExprStatement
  | SCXX_Assignment
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

type SCXXState = Tree (State UUID (Node SimpleCXXGrammar String))

scxx_funcdef :: String -> String -> [(String, String)] -> [SCXXState] -> SCXXState
scxx_funcdef rettype name params content =
    (Tree (node name SCXX_FuncDef) [
        scxx_type rettype,
        scxx_parametersdef params,
        scxx_statements content
    ])

scxx_parametersdef :: [(String, String)] -> SCXXState
scxx_parametersdef params = Tree (node mempty SCXX_ParametersDef) $ scxx_expr.(uncurry scxx_vardecl) <$> params

scxx_args :: [SCXXState] -> SCXXState
scxx_args params = Tree (node mempty SCXX_Args) $ scxx_expr.scxx_expr <$> params

scxx_statements :: [SCXXState] -> SCXXState
scxx_statements statements = Tree (node mempty SCXX_Statements) statements

scxx_exprstatement :: SCXXState -> SCXXState
scxx_exprstatement expression = Tree (node mempty SCXX_ExprStatement) [scxx_expr expression]

scxx_assignment :: SCXXState -> String -> SCXXState -> SCXXState
scxx_assignment lhs op expr = Tree (node op SCXX_Assignment) [lhs, scxx_expr expr]

scxx_return :: SCXXState -> SCXXState
scxx_return expr = Tree (node mempty SCXX_Return) [scxx_expr expr]

scxx_condition :: SCXXState -> [SCXXState] -> SCXXState
scxx_condition cond block =
    Tree (node mempty SCXX_Condition) [
        scxx_expr cond,
        scxx_statements block
    ]

scxx_funccall :: String -> [SCXXState] -> SCXXState
scxx_funccall name params =
    Tree (node name SCXX_FuncCall) [
        scxx_args params
    ]

scxx_expr :: SCXXState -> SCXXState
scxx_expr inner = Tree (node "Expression" SCXX_Expression) [inner]

scxx_unaryop :: String -> SCXXState -> SCXXState
scxx_unaryop op expr = Tree (node op SCXX_UnaryOp) [scxx_expr expr]

scxx_binaryop :: SCXXState -> String -> SCXXState -> SCXXState
scxx_binaryop lhs op rhs =
    Tree (node op SCXX_BinaryOp) [
        scxx_expr lhs,
        scxx_expr rhs
    ]

scxx_vardecl :: String -> String -> SCXXState
scxx_vardecl vartype varname =
    (Tree (node mempty SCXX_VarDecl)) [
        scxx_type vartype,
        scxx_literal varname
    ]

scxx_varref :: String -> SCXXState
scxx_varref name = Tree (node name SCXX_VarRef) []

scxx_literal :: String -> SCXXState
scxx_literal val = Tree (node val SCXX_Literal) []

scxx_type :: String -> SCXXState
scxx_type val = Tree (node val SCXX_Type) []

scxx_file :: String -> [SCXXState] -> SCXXState
scxx_file name content = Tree (node name SCXX_File) content

instance Grammar SimpleCXXGrammar where
    nodetypeof SCXX_FuncDef = Treeoptional
    nodetypeof SCXX_Return = Treeoptional
    nodetypeof SCXX_File = Treeoptional
    nodetypeof SCXX_ExprStatement = Treeoptional
    nodetypeof SCXX_VarDecl = Mandatory
    nodetypeof SCXX_Condition = Optional
    nodetypeof SCXX_UnaryOp = Optional --Mandatory
    nodetypeof SCXX_Type = Optional
    nodetypeof SCXX_Expression = Mandatory -- Virtual
    nodetypeof SCXX_ParametersDef = Mandatory
    nodetypeof SCXX_Args = Mandatory
    nodetypeof SCXX_Statements = Mandatory
    nodetypeof SCXX_BinaryOp = Optional
    nodetypeof SCXX_FuncCall = Optional
    nodetypeof SCXX_VarRef = Optional -- leaf type
    nodetypeof SCXX_Literal = Optional -- leaf type
    nodetypeof SCXX_Assignment = Mandatory

    prettyPrint i indentGenerator prtStrWithContext prtNode (Tree n children) =
        mconcat $
        let indentLen = 2
            indent = mappend i $ indentGenerator n indentLen
            prtStr = prtStrWithContext n
            me = prtNode n
            showList sep l = showList' (prtStr sep) l
            showList' sep l = mconcat $ intersperse sep $ (showCodeAs indent indentGenerator prtStrWithContext prtNode) <$> l
            showListNoIndentIncrease sep l = mconcat $ intersperse (prtStr sep) $ showCodeAs i indentGenerator prtStrWithContext prtNode <$> l
            showHead = showCodeAs indent indentGenerator prtStrWithContext prtNode $ head children
            in case rule n of
                SCXX_FuncDef -> [indent, showHead, prtStr " ", me, showList " " $ tail children]
                SCXX_ParametersDef -> [prtStr "(", showList ", " children, prtStr ")"]
                SCXX_Args -> [prtStr "(", showList ", " children, prtStr ")"]
                SCXX_Statements -> [
                    prtStr "\n",
                    i, prtStr "{\n",
                    if null children
                    then mempty
                    else (mappend (showListNoIndentIncrease "\n" children) (prtStr "\n")),
                    i, prtStr "}"]
                SCXX_ExprStatement -> [indent, showList " " children, prtStr ";"]
                SCXX_Assignment -> [showList' (mconcat [prtStr" ",me,prtStr" "]) children]
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
