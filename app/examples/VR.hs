module VR where

import Control.Monad.State
import UUID
import Tree
import AST
import Edits
import Propositions
import FeatureTrace
import FeatureColour
import SimpleCXX
import Example
import System.Terminal
import Data.Maybe ( fromJust )

type SSCXXAST = SCXXAST String

feature_Setup :: Feature
feature_Setup = toFeature "Setup"
feature_VR :: Feature
feature_VR = toFeature "VR"

featureColourPalette :: (MonadColorPrinter m) => FeatureColourPalette m
featureColourPalette feature 
    | feature == feature_Setup = green
    | feature == feature_VR = cyan
    | otherwise = red

vr0 :: State UUID SSCXXAST
vr0 = sequence
    (Tree (node "main" SCXX_FuncDef) [
        (Tree (node "void" SCXX_Type) []),
        (Tree (node "params" SCXX_Parameters) [
            (Tree (node "param" SCXX_VarDecl) [
                (Tree (node "String[]" SCXX_Type) []),
                (Tree (node "args" SCXX_Literal) [])
            ])
        ]),
        (Tree (node "body" SCXX_Statements) [
            (Tree (node mempty SCXX_ExprStatement) [
                (Tree (node "runGame" SCXX_FuncCall) [
                    (Tree (node "params" SCXX_Parameters) [])
                ])
            ])
        ])
    ])

vr_setupCall = sequence
    (Tree (node mempty SCXX_ExprStatement) [
        (Tree (node "setup" SCXX_FuncCall) [
            (Tree (node "params" SCXX_Parameters) [
                (Tree (node "param" SCXX_Expression) [
                    (Tree (node "args" SCXX_VarRef) [])
                ])
            ])
        ])
    ])

vr_cond = sequence
    (Tree (node "if" SCXX_Condition) [
        (Tree (node "cond" SCXX_Expression) [
            (Tree (node "success" SCXX_VarRef) [])
        ]),
        (Tree (node "body" SCXX_Statements) [
            -- Here, other statements have to be entered later.
        ])
    ])

vr_vrsetupCall = sequence
    (Tree (node mempty SCXX_ExprStatement) [
        (Tree (node "vr_setup" SCXX_FuncCall) [
            (Tree (node "params" SCXX_Parameters) [
                (Tree (node "param" SCXX_Expression) [
                    (Tree (node "args" SCXX_VarRef) [])
                ])
            ])
        ])
    ])

vrExample :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
vrExample =
    do
        tree0 <- vr0
        tree_vr_setupCall <- vr_setupCall
        tree_vr_cond <- vr_cond
        tree_vr_vrsetupCall <- vr_vrsetupCall
        let
            id_tree0_body = uuidOf . fromJust $ findWithValue "body" tree0
            id_runGame = uuidOf . fromJust $ findWithNode ((SCXX_ExprStatement==).rule) tree0
            id_vr_cond_body = uuidOf . fromJust $ findWithNode ((SCXX_Statements==).rule) tree_vr_cond
        return Example {
            startTrace = emptyTrace,
            startTree = tree0,
            editscript = [
                edit_ins_tree tree_vr_setupCall id_tree0_body 0
              , edit_ins_tree tree_vr_cond id_tree0_body 1
              , edit_move_tree id_runGame id_vr_cond_body 0
              , edit_del_tree $ uuidOf tree_vr_setupCall
              , edit_ins_tree tree_vr_vrsetupCall id_tree0_body 0
            ],
            featurecontexts = [
                Just $ PVariable feature_Setup
              , Just $ PVariable feature_Setup
              , Nothing
              , Just $ PVariable feature_VR
              , Just $ PVariable feature_VR
            ],
            colours = featureColourPalette
        }