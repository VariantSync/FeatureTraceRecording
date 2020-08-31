module Div where

import Control.Monad.State
import UUID
import Tree
import AST

div0 :: State UUID (AST String)
div0 = sequence
    (Tree (node "IchKannTeilen.cpp" ASTT_File) [
        (Tree (node "reciprocal" ASTT_FuncDef) [
            (Tree (node "double" ASTT_Type) []),
            (Tree (node "params" ASTT_Parameters) [
                (Tree (node "param" ASTT_VarDecl) [
                    (Tree (node "double" ASTT_Type) []),
                    (Tree (node "x" ASTT_Literal) [])
                ])
            ]),
            (Tree (node "body" ASTT_Statements) [
                (Tree (node "return" ASTT_Return) [
                    (Tree (node "Expression" ASTT_Expression) [
                        (Tree (node "/" ASTT_BinaryOp) [
                            (Tree (node "1.0" ASTT_Literal) []),
                            (Tree (node "b" ASTT_VarRef) [])
                        ])
                    ])
                ])
            ])
        ])
    ])

div_assert :: State UUID (AST String)
div_assert = sequence
    (Tree (node "assert" ASTT_FuncCall) [
        (Tree (node "params" ASTT_Parameters) [
            (Tree (node "param" ASTT_Expression) [
                (Tree (node "!=" ASTT_BinaryOp) [
                    (Tree (node "x" ASTT_VarRef) []),
                    (Tree (node "0" ASTT_Literal) [])
                ])
            ])
        ])
    ])

div_error :: State UUID (AST String)
div_error = sequence
    (Tree (node "error" ASTT_FuncCall) [
        (Tree (node "params" ASTT_Parameters) [
            (Tree (node "param" ASTT_Expression) [
                (Tree (node "\"Cannot divide by 0!\"" ASTT_Literal) [])
            ])
        ])
    ])

div_condition :: State UUID (AST String)
div_condition = sequence
    (Tree (node "if" ASTT_Condition) [
        (Tree (node "cond" ASTT_Expression) [
            (Tree (node "==" ASTT_BinaryOp) [
                (Tree (node "x" ASTT_VarRef) []),
                (Tree (node "0" ASTT_Literal) [])
            ])
        ]),
        (Tree (node "body" ASTT_Statements) [
            -- Here, other statements have to be entered later.
        ])
    ])

div_div :: State UUID (AST String)
div_div = sequence
    (Tree (node "div" ASTT_FuncDef) [
        (Tree (node "double" ASTT_Type) []),
        (Tree (node "params" ASTT_Parameters) [
            (Tree (node "param" ASTT_VarDecl) [
                (Tree (node "double" ASTT_Type) []),
                (Tree (node "a" ASTT_Literal) [])
            ]),
            (Tree (node "param" ASTT_VarDecl) [
                (Tree (node "double" ASTT_Type) []),
                (Tree (node "b" ASTT_Literal) [])
            ])
        ]),
        (Tree (node "body" ASTT_Statements) [])
    ])
