module SimpleJava where

import Tree
import Control.Monad.State ( State )
import UUID ( UUID )
import AST
import Grammar
import ASTPrettyPrinter
import Data.List ( intersperse )

type SJavaAST a = AST SimpleJavaGrammar a
type SSJavaAST = SJavaAST String

{- Simplified grammer for pseudo C++ -}
data SimpleJavaGrammar = 
    SJava_MethodDef
  | SJava_ParametersDef
  | SJava_Args
  | SJava_Statements
  | SJava_ExprStatement
  | SJava_Assignment
  | SJava_Return
  | SJava_Condition
  | SJava_FuncCall
  | SJava_Expression
  | SJava_UnaryOp
  | SJava_BinaryOp
  | SJava_VarDecl
  | SJava_VarRef
  | SJava_Literal
  | SJava_Type
  | SJava_File
  deriving (Eq, Show)

type SJavaState = Tree (State UUID (Node SimpleJavaGrammar String))

{- Grammar rules to build the AST -}

sjava_methoddef :: String -> String -> [(String, String)] -> [SJavaState] -> SJavaState
sjava_methoddef rettype name params content =
    (Tree (node name SJava_MethodDef) [
        sjava_type rettype,
        sjava_parametersdef params,
        sjava_statements content
    ])

sjava_parametersdef :: [(String, String)] -> SJavaState
sjava_parametersdef params = Tree (node mempty SJava_ParametersDef) $ sjava_expr.(uncurry sjava_vardecl) <$> params

sjava_args :: [SJavaState] -> SJavaState
sjava_args params = Tree (node mempty SJava_Args) $ sjava_expr.sjava_expr <$> params

sjava_statements :: [SJavaState] -> SJavaState
sjava_statements statements = Tree (node mempty SJava_Statements) statements

sjava_exprstatement :: SJavaState -> SJavaState
sjava_exprstatement expression = Tree (node mempty SJava_ExprStatement) [sjava_expr expression]

sjava_assignment :: SJavaState -> String -> SJavaState -> SJavaState
sjava_assignment lhs op expr = Tree (node op SJava_Assignment) [lhs, sjava_expr expr]

sjava_return :: SJavaState -> SJavaState
sjava_return expr = Tree (node mempty SJava_Return) [sjava_expr expr]

sjava_condition :: SJavaState -> [SJavaState] -> SJavaState
sjava_condition cond block =
    Tree (node mempty SJava_Condition) [
        sjava_expr cond,
        sjava_statements block
    ]

sjava_funccall :: String -> [SJavaState] -> SJavaState
sjava_funccall name params =
    Tree (node name SJava_FuncCall) [
        sjava_args params
    ]

sjava_expr :: SJavaState -> SJavaState
sjava_expr inner = Tree (node "Expression" SJava_Expression) [inner]

sjava_unaryop :: String -> SJavaState -> SJavaState
sjava_unaryop op expr = Tree (node op SJava_UnaryOp) [sjava_expr expr]

sjava_binaryop :: SJavaState -> String -> SJavaState -> SJavaState
sjava_binaryop lhs op rhs =
    Tree (node op SJava_BinaryOp) [
        sjava_expr lhs,
        sjava_expr rhs
    ]

sjava_vardecl :: String -> String -> SJavaState
sjava_vardecl vartype varname =
    (Tree (node mempty SJava_VarDecl)) [
        sjava_type vartype,
        sjava_literal varname
    ]

sjava_varref :: String -> SJavaState
sjava_varref name = Tree (node name SJava_VarRef) []

sjava_literal :: String -> SJavaState
sjava_literal val = Tree (node val SJava_Literal) []

sjava_type :: String -> SJavaState
sjava_type val = Tree (node val SJava_Type) []

sjava_file :: String -> [SJavaState] -> SJavaState
sjava_file name content = Tree (node name SJava_File) content

{- Define optionality for all node types in our C++ grammar. -}
instance Grammar SimpleJavaGrammar where
    nodetypeof SJava_MethodDef = Optional
    nodetypeof SJava_Return = Optional --leaf
    nodetypeof SJava_File = Optional
    nodetypeof SJava_ExprStatement = Optional
    nodetypeof SJava_VarDecl = Mandatory
    nodetypeof SJava_Condition = Wrapper
    nodetypeof SJava_UnaryOp = Wrapper
    nodetypeof SJava_Type = Optional
    nodetypeof SJava_Expression = Mandatory
    nodetypeof SJava_ParametersDef = Mandatory
    nodetypeof SJava_Args = Mandatory
    nodetypeof SJava_Statements = Mandatory
    nodetypeof SJava_BinaryOp = Optional
    nodetypeof SJava_FuncCall = Optional
    nodetypeof SJava_VarRef = Optional -- leaf
    nodetypeof SJava_Literal = Optional -- leaf
    nodetypeof SJava_Assignment = Mandatory

instance ASTPrettyPrinter SimpleJavaGrammar where
    {- print AST as source code -}
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
            in case grammartype n of
                SJava_MethodDef -> [indent, showHead, prtStr " ", me, showList " " $ tail children]
                SJava_ParametersDef -> [prtStr "(", showList ", " children, prtStr ")"]
                SJava_Args -> [prtStr "(", showList ", " children, prtStr ")"]
                SJava_Statements -> [
                    -- prtStr "\n",
                    -- i,
                    prtStr "{\n",
                    if null children
                    then mempty
                    else (mappend (showListNoIndentIncrease "\n" children) (prtStr "\n")),
                    i, prtStr "}"]
                SJava_ExprStatement -> [indent, showList " " children, prtStr ";"]
                SJava_Assignment -> [showList' (mconcat [prtStr" ",me,prtStr" "]) children]
                SJava_Return -> [indent, prtStr "return ", showList " " children, prtStr ";"]
                SJava_Condition -> [indent, prtStr "if (", showHead, prtStr ") ", showList " " $ tail children]
                SJava_FuncCall -> [me, showList ", " children]
                SJava_Expression -> return $ if length children == 1 then showHead else error "Expressios can only have one child"
                SJava_UnaryOp -> return $ if length children == 1 then mappend me showHead else error "Unary operations can only have one child"
                SJava_BinaryOp -> if length children == 2 then [showHead, prtStr " ", me, prtStr " ", showCodeAs indent indentGenerator prtStrWithContext prtNode $ head $ tail children] else error "Binary operations must have exactly two children"
                SJava_VarDecl -> return $ showList " " children
                SJava_VarRef -> return me
                SJava_Literal -> return me
                SJava_Type -> return me
                SJava_File -> [indent, prtStr "FILE [", me, prtStr $ "] {\n", showList "\n" children, prtStr "\n", indent, prtStr "}"]
