module Div where

import Example
import Control.Monad.State
import UUID
import Tree
import AST
import Edits
import Propositions
import FeatureTrace
import FeatureColour
import SimpleCXX
import System.Terminal
import Data.Maybe ( fromJust )

feature_Debug :: Feature
feature_Debug = toFeature "Debug"
feature_Reciprocal :: Feature
feature_Reciprocal = toFeature "Reciprocal"
feature_Division :: Feature
feature_Division = toFeature "Division"

featureColourPalette :: (MonadColorPrinter m) => FeatureColourPalette m
featureColourPalette feature 
    | feature == feature_Debug = green
    | feature == feature_Reciprocal = yellow
    | feature == feature_Division = cyan
    | otherwise = red

div0 :: State UUID SSCXXAST
div0 = sequence
    (Tree (node "Divider9000.cpp" SCXX_File) [
        (Tree (node "reciprocal" SCXX_FuncDef) [
            (Tree (node "double" SCXX_Type) []),
            (Tree (node "params" SCXX_ParametersDef) [
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
            (Tree (node "params" SCXX_Args) [
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
            (Tree (node "params" SCXX_Args) [
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
        (Tree (node "params" SCXX_ParametersDef) [
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
                (Tree (node "params" SCXX_Args) [
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

divExample :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
divExample =
    do
        tree0 <- div0
        tree_assert <- div_assert
        tree_error <- div_error
        tree_condition <- div_condition
        tree_div <- div_div
        tree_reciprocal_return <- div_reciprocal_return
        let
            id_reciprocal_body = uuidOf . fromJust $ findByRule SCXX_Statements tree0
            tree_return = fromJust $ findByRule SCXX_Return tree0
            id_cond_body = uuidOf . fromJust $ findByRule SCXX_Statements tree_condition
            id_div_body = uuidOf . fromJust $ findByRule SCXX_Statements tree_div
            tree_x_condexpr = fromJust $ findByValue "x" tree_condition
            tree_x_return = fromJust $ findByValue "x" tree_return
            tree_1_return = fromJust $ findByValue "1.0" tree_return
        return Example {
            Example.name = "Div",
            colours = defaultFeatureFormulaColouring featureColourPalette,
            startTrace = emptyTrace,
            startTree = tree0,
            editscript = [
                edit_ins_tree tree_assert id_reciprocal_body 0
              , edit_ins_partial tree_condition id_reciprocal_body 0 0 id_cond_body 0
              , edit_del_tree (uuidOf tree_assert)
              , edit_ins_tree tree_error id_cond_body 0
              , edit_ins_tree tree_div (uuidOf tree0) 0
              , edit_move_tree (uuidOf tree_condition) id_div_body 0
              , edit_move_tree (uuidOf tree_return) id_div_body 1 -- now we are done with div5.txt
              , edit_update (uuidOf tree_x_condexpr) (rule $ element $ tree_x_condexpr) "b"
              , edit_update (uuidOf tree_x_return) (rule $ element $ tree_x_return) "b"
              , edit_update (uuidOf tree_1_return) SCXX_VarRef "a"
              , edit_ins_tree tree_reciprocal_return id_reciprocal_body 0
            ],
            featurecontexts = [
                Just $ PVariable feature_Debug
              , Just $ PVariable feature_Reciprocal
              , Just $ PVariable feature_Reciprocal -- Error by user. Should actually be PTrue
              , Just $ PVariable feature_Reciprocal
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Division
              , Just $ PVariable feature_Reciprocal
            ]
        }