module Div where

import Control.Monad.State
import UUID
import Tree
import AST
import FeatureTrace
import SimpleCXX
import System.Terminal

type SSCXXAST = SCXXAST String

feature_Debug :: Feature
feature_Debug = toFeature "Debug"
feature_Reciprocal :: Feature
feature_Reciprocal = toFeature "Reciprocal"
feature_Division :: Feature
feature_Division = toFeature "Division"

colourOf :: (MonadColorPrinter m) => Feature -> Color m
colourOf feature 
    | feature == feature_Debug = green
    | feature == feature_Reciprocal = yellow
    | feature == feature_Division = cyan
    | otherwise = red

div0 :: State UUID SSCXXAST
div0 = sequence
    (Tree (node "Divider9000.cpp" SCXX_File) [
        (Tree (node "reciprocal" SCXX_FuncDef) [
            (Tree (node "double" SCXX_Type) []),
            (Tree (node "params" SCXX_Parameters) [
                (Tree (node "param" SCXX_VarDecl) [
                    (Tree (node "double" SCXX_Type) []),
                    (Tree (node "x" SCXX_Literal) [])
                ])
            ]),
            (Tree (node "body" SCXX_Statements) [
                (Tree (node "return" SCXX_Return) [
                    (Tree (node "Expression" SCXX_Expression) [
                        (Tree (node "/" SCXX_BinaryOp) [
                            (Tree (node "1.0" SCXX_Literal) []),
                            (Tree (node "x" SCXX_VarRef) [])
                        ])
                    ])
                ])
            ])
        ])
    ])

div_assert :: State UUID SSCXXAST
div_assert = sequence
    (Tree (node mempty SCXX_ExprStatement) [
        (Tree (node "assert" SCXX_FuncCall) [
            (Tree (node "params" SCXX_Parameters) [
                (Tree (node "param" SCXX_Expression) [
                    (Tree (node "!=" SCXX_BinaryOp) [
                        (Tree (node "x" SCXX_VarRef) []),
                        (Tree (node "0" SCXX_Literal) [])
                    ])
                ])
            ])
        ])
    ])

div_error :: State UUID SSCXXAST
div_error = sequence
    (Tree (node mempty SCXX_ExprStatement) [
        (Tree (node "error" SCXX_FuncCall) [
            (Tree (node "params" SCXX_Parameters) [
                (Tree (node "param" SCXX_Expression) [
                    (Tree (node "\"Cannot divide by 0!\"" SCXX_Literal) [])
                ])
            ])
        ])
    ])

div_condition :: State UUID SSCXXAST
div_condition = sequence
    (Tree (node "if" SCXX_Condition) [
        (Tree (node "cond" SCXX_Expression) [
            (Tree (node "==" SCXX_BinaryOp) [
                (Tree (node "x" SCXX_VarRef) []),
                (Tree (node "0" SCXX_Literal) [])
            ])
        ]),
        (Tree (node "body" SCXX_Statements) [
            -- Here, other statements have to be entered later.
        ])
    ])

div_div :: State UUID SSCXXAST
div_div = sequence
    (Tree (node "div" SCXX_FuncDef) [
        (Tree (node "double" SCXX_Type) []),
        (Tree (node "params" SCXX_Parameters) [
            (Tree (node "param" SCXX_VarDecl) [
                (Tree (node "double" SCXX_Type) []),
                (Tree (node "a" SCXX_Literal) [])
            ]),
            (Tree (node "param" SCXX_VarDecl) [
                (Tree (node "double" SCXX_Type) []),
                (Tree (node "b" SCXX_Literal) [])
            ])
        ]),
        (Tree (node "body" SCXX_Statements) [])
    ])

div_reciprocal_return :: State UUID SSCXXAST
div_reciprocal_return = sequence
    (Tree (node "return" SCXX_Return) [
        (Tree (node "Expression" SCXX_Expression) [
            (Tree (node "div" SCXX_FuncCall) [
                (Tree (node "params" SCXX_Parameters) [
                    (Tree (node "param" SCXX_Expression) [
                        (Tree (node "1.0" SCXX_Literal) [])
                    ]),
                    (Tree (node "param" SCXX_Expression) [
                        (Tree (node "x" SCXX_VarRef) [])
                    ])
                ])
            ])
        ])
    ])