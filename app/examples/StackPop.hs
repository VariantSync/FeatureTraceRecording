module StackPop where

import Control.Monad.State ( State )
import UUID ( UUID )
import Tree
import AST
import Edits
import Propositions
import FeatureTrace
import FeatureColour
import SimpleCXX
import Example ( Example(..) )
import System.Terminal
    ( MonadColorPrinter(..) )
import Data.Maybe ( fromJust )

feature_Stack :: Feature
feature_Stack = toFeature "Stack"
feature_SafeStack :: Feature
feature_SafeStack = toFeature "SafeStack"
feature_ImmutableStack :: Feature
feature_ImmutableStack = toFeature "ImmutableStack"

featureColourPalette :: (MonadColorPrinter m) => FeatureColourPalette m
featureColourPalette feature 
    | feature == feature_Stack = yellow
    | feature == feature_SafeStack = green
    | feature == feature_ImmutableStack = cyan
    | otherwise = red

startTree :: State UUID SSCXXAST
startTree = sequence $
    scxx_funcdef "void" "pop" [] [
        scxx_exprstatement $ scxx_assignment (scxx_varref "storage[head--]") "=" (scxx_literal "null")
    ]

condTree :: State UUID SSCXXAST
condTree = sequence $ scxx_condition (scxx_unaryop "!" $ scxx_funccall "empty" []) []

cloneDef :: State UUID SSCXXAST
cloneDef = sequence $ scxx_exprstatement $ scxx_assignment (scxx_vardecl "Stack<T>" "clone") "=" (scxx_funccall "clone" [])

cloneStorage :: State UUID SSCXXAST
cloneStorage = sequence $ scxx_exprstatement $ scxx_assignment (scxx_varref "clone.storage[clone.head--]") "=" (scxx_literal "null")

cloneRetStatement :: State UUID SSCXXAST
cloneRetStatement = sequence $ scxx_return $ scxx_varref "clone"

newReturnType :: String
newReturnType = "Stack<T>"

example :: (MonadColorPrinter m) => State UUID (Example m SimpleCXXGrammar String)
example =
    do
        tree_start <- StackPop.startTree
        tree_cond <- StackPop.condTree
        tree_clonedef <- StackPop.cloneDef
        tree_clonestorage <- StackPop.cloneStorage
        tree_cloneretstatement <- StackPop.cloneRetStatement
        let
            id_tree_start_body = uuidOf . fromJust $ findWithValue "body" tree_start
            id_tree_start_storage = uuidOf . fromJust $ findWithNode ((SCXX_ExprStatement==).rule) tree_start
            id_tree_cond_body = uuidOf . fromJust $ findWithValue "body" tree_cond
            id_tree_start_ret = uuidOf . fromJust $ findWithNode ((SCXX_Type==).rule) tree_start
        return Example {
            Example.startTrace = emptyTrace,
            Example.startTree = tree_start,
            editscript = [
                edit_ins_tree tree_cond id_tree_start_body 0
              , edit_move_tree id_tree_start_storage id_tree_cond_body 0
              , edit_del_tree id_tree_start_storage
              , edit_ins_tree tree_clonestorage id_tree_cond_body 0
              , edit_ins_tree tree_clonedef id_tree_start_body 0
              , edit_ins_tree tree_cloneretstatement id_tree_start_body 2
              , edit_update id_tree_start_ret SCXX_Type newReturnType
            ],
            featurecontexts = [
                Just $ PVariable feature_SafeStack
              , Nothing
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
            ],
            colours = featureColourPalette
        }