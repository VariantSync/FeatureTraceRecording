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
vr0 = sequence $
    scxx_funcdef "void" "main" [("String[]", "args")] [
        scxx_exprstatement $ scxx_funccall "runGame" []
    ]

vr_setupCall :: State UUID SSCXXAST
vr_setupCall = sequence $
    scxx_exprstatement $ 
    scxx_assignment (scxx_vardecl "bool" "success") "=" (scxx_funccall "setup" [scxx_varref "args"])

vr_cond :: State UUID SSCXXAST
vr_cond = sequence $ scxx_condition (scxx_varref "success") []

vr_vrsetupCall :: State UUID SSCXXAST
vr_vrsetupCall = sequence $
    scxx_exprstatement $ 
    scxx_assignment (scxx_vardecl "bool" "success") "=" (scxx_funccall "vr_setup" [scxx_varref "args"])

vrExample :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
vrExample =
    do
        tree0 <- vr0
        tree_vr_setupCall <- vr_setupCall
        tree_vr_cond <- vr_cond
        tree_vr_vrsetupCall <- vr_vrsetupCall
        let
            id_tree0_body = uuidOf . fromJust $ findByGrammarType SCXX_Statements tree0
            id_runGame = uuidOf . fromJust $ findByGrammarType SCXX_ExprStatement tree0
            id_vr_cond_body = uuidOf . fromJust $ findByGrammarType SCXX_Statements tree_vr_cond
        return Example {
            Example.name = "VR",
            colours = defaultFeatureFormulaColouring featureColourPalette,
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
            ]
        }