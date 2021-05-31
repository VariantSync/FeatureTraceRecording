-- | Example implementation of a context free grammar.
-- This example represents a simplified subset of the Java programming language.
-- It contains the 'Grammar' implementation as well as functions to construct an AST in this sub-language.
module SimpleJava where

import Tree
import Control.Monad.State ( State )
import UUID ( UUID )
import AST
import Grammar
import ASTPrettyPrinter
import Data.List ( intersperse )

-- | Simplified subset of the Java grammar.
data SimpleJavaGrammar = 
    SJava_MethodDef -- ^ Rule for method definitions.
  | SJava_ParametersDef -- ^ Rule for a parameters list (e.g., in a method declaration).
  | SJava_Args -- ^ Rule for arguments passed to a method call (e.g., @(1 + 2, "arg2", null)@).
  | SJava_Statements -- ^ Rule for a sequential list of statements (e.g., inside a method).
  | SJava_ExprStatement -- ^ Rule for a statement that is a single expression (e.g., @x += 3@ that also returns the new value of @x@ is an expression statement).
  | SJava_Assignment -- ^ Rule for assignments of variables (e.g., @x = 3@).
  | SJava_Return -- ^ Rule for @return@ statements.
  | SJava_Condition -- ^ Rule for conditions (@if@).
  | SJava_FuncCall -- ^ Rule for function calls.
  | SJava_Expression -- ^ Rule for expressions (e.g., @1 + 1@).
  | SJava_UnaryOp -- ^ Rule for unary operations (e.g., @!@ or @-@).
  | SJava_BinaryOp -- ^ Rule for binary operations (e.g., @+@ or @*@).
  | SJava_VarDecl -- ^ Rule for variable declarations (e.g., @int x;@).
  | SJava_VarRef -- ^ Rule for variable references.
  | SJava_Literal -- ^ Rule for literals.
  | SJava_Type -- ^ Rule for types (e.g., @int@ in @int x;@).
  | SJava_File  -- ^ Rule that represents an entire file. Usually the root of the AST.
  deriving (Eq, Show)

-- | An AST build from the SimpleJavaGrammar.
type SJavaAST a = AST SimpleJavaGrammar a
-- | An AST build from the SimpleJavaGrammar containing Strings as values.
type SSJavaAST = SJavaAST String
-- | A 'SSJavaAST' whose nodes are not yet assigned their UUIDs (they are still states waiting for evaluation).
type SJavaState = Tree (State UUID (Node SimpleJavaGrammar String))

{- Grammar rules to build the AST -}

-- | Construct a method definition with given (1) return type, (2) name, (3) parameters (pairs of type and name), and (4) the given content (list of statements).
sjava_methoddef :: String -> String -> [(String, String)] -> [SJavaState] -> SJavaState
sjava_methoddef rettype name params content =
    (Tree (node name SJava_MethodDef) [
        sjava_type rettype,
        sjava_parametersdef params,
        sjava_statements content
    ])

-- | Construct a definition of parameters subtree from the given pairs of type and name.
sjava_parametersdef :: [(String, String)] -> SJavaState
sjava_parametersdef params = Tree (node mempty SJava_ParametersDef) $ sjava_expr.(uncurry sjava_vardecl) <$> params

-- | Construct an arguments subtree from the given list of subtrees (e.g., @(1 + 2, "arg2", null)@).
sjava_args :: [SJavaState] -> SJavaState
sjava_args params = Tree (node mempty SJava_Args) $ sjava_expr.sjava_expr <$> params

-- | Construct a statements block from a list of statements.
sjava_statements :: [SJavaState] -> SJavaState
sjava_statements statements = Tree (node mempty SJava_Statements) statements

-- | Wrap the given subtree in an expression statement. The given tree should be an expression.
-- Pseudocode example: @sjava_exprstatement 'x += 3  ==  'x + 3;'@.
sjava_exprstatement :: SJavaState -> SJavaState
sjava_exprstatement expression = Tree (node mempty SJava_ExprStatement) [sjava_expr expression]

-- | Constructs an assignment where parameter
-- (1) represents the left side of the assignment (e.g., a variable that should be assigned a value),
-- (2) is the operator's name to use (e.g., @"="@), and
-- (3) is the expression to assign (e.g. "1 + 2").
sjava_assignment :: SJavaState -> String -> SJavaState -> SJavaState
sjava_assignment lhs op expr = Tree (node op SJava_Assignment) [lhs, sjava_expr expr]

-- | Constructs a return statement that returns the given expression.
sjava_return :: SJavaState -> SJavaState
sjava_return expr = Tree (node mempty SJava_Return) [sjava_expr expr]

-- | Constructs an if-block (without else case) with the given condition (first argument) and sequence of statements to run when the condition is met.
sjava_condition :: SJavaState -> [SJavaState] -> SJavaState
sjava_condition cond block =
    Tree (node mempty SJava_Condition) [
        sjava_expr cond,
        sjava_statements block
    ]

-- | Constructs a call to a function the given name and a list of arguments to pass.
sjava_funccall :: String -> [SJavaState] -> SJavaState
sjava_funccall name params =
    Tree (node name SJava_FuncCall) [
        sjava_args params
    ]

-- | Wraps an tree in an expression.
-- For example, a literal is an expression.
-- Thus a literal can be used anywhere an expression required but should be wrapped into an expression first.
sjava_expr :: SJavaState -> SJavaState
sjava_expr inner = Tree (node "Expression" SJava_Expression) [inner]

-- | Constructs a unary operation (e.g. @-3@) from a given name (@-@) that is applied to the given expression (@3@).
sjava_unaryop :: String -> SJavaState -> SJavaState
sjava_unaryop op expr = Tree (node op SJava_UnaryOp) [sjava_expr expr]

-- | Constructs a binary operator from a given name and two expressions.
sjava_binaryop :: SJavaState -> String -> SJavaState -> SJavaState
sjava_binaryop lhs op rhs =
    Tree (node op SJava_BinaryOp) [
        sjava_expr lhs,
        sjava_expr rhs
    ]

-- | Constructs a variable declaration (e.g., @int x;@) from a type (e.g., @int@) and a name (e.g., @x@).
sjava_vardecl :: String -> String -> SJavaState
sjava_vardecl vartype varname =
    (Tree (node mempty SJava_VarDecl)) [
        sjava_type vartype,
        sjava_literal varname
    ]

-- | Constructs a reference to a variable with the given name. The returned tree is a leaf.
sjava_varref :: String -> SJavaState
sjava_varref name = Tree (node name SJava_VarRef) []

-- | Constructs a literal with the given name. The returned tree is a leaf.
sjava_literal :: String -> SJavaState
sjava_literal val = Tree (node val SJava_Literal) []

-- | Constructs a type with the given name. The returned tree is a leaf.
sjava_type :: String -> SJavaState
sjava_type val = Tree (node val SJava_Type) []

-- | Constructs a file with the given name and content.
sjava_file :: String -> [SJavaState] -> SJavaState
sjava_file name content = Tree (node name SJava_File) content

{- Define optionality for all node types in our Java grammar. -}
instance Grammar SimpleJavaGrammar where
    nodetypeof SJava_MethodDef = Optional
    nodetypeof SJava_Return = Optional --leaf
    nodetypeof SJava_File = Optional
    nodetypeof SJava_ExprStatement = Optional
    nodetypeof SJava_VarDecl = Mandatory
    nodetypeof SJava_Condition = Wrapper
    nodetypeof SJava_UnaryOp = Wrapper
    nodetypeof SJava_Type = Optional -- leaf
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
    -- | Prints AST as source code
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
