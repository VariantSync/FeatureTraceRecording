module StackPopAlice where

import Control.Monad.State ( State )
import UUID ( UUID )
import Tree
import AST
import Edits
import Propositions
import Feature
import FeatureTrace
import FeatureColour
import SimpleCXX
import System.Terminal
    ( MonadColorPrinter(..) )
import Data.Maybe ( fromJust )

import Example

-- feature_Stack :: Feature
-- feature_Stack = toFeature "Stack"
feature_SafeStack :: Feature
feature_SafeStack = toFeature "SafeStack"
feature_ImmutableStack :: Feature
feature_ImmutableStack = toFeature "ImmutableStack"

-- featureColourPalette :: MonadColorPrinter m => Feature -> Color m
-- featureColourPalette feature 
--     -- | feature == feature_Stack = yellow
--     | feature == feature_SafeStack = green
--     | feature == feature_ImmutableStack = yellow
--     | otherwise = red

featurecolours :: MonadColorPrinter m => FeatureFormulaColourPalette m
featurecolours p
    | p == (Just $ PNot $ PVariable $ feature_ImmutableStack) = magenta
    | p == (Just $ PVariable $ feature_SafeStack) = green
    | p == (Just $ PVariable $ feature_ImmutableStack) = yellow
    | otherwise = white

startTree :: State UUID SSCXXAST
startTree = sequence $
    scxx_funcdef "void" "pop" [] [
        scxx_exprstatement $ scxx_assignment (scxx_varref "storage[head--]") "=" (scxx_literal "null")
    ]

condTree :: State UUID SSCXXAST
condTree = sequence $ scxx_condition (scxx_unaryop "!" $ scxx_funccall "empty" []) []

cloneDef :: State UUID SSCXXAST
cloneDef = sequence $ scxx_exprstatement $ scxx_assignment (scxx_vardecl "Stack<T>" "c") "=" (scxx_funccall "clone" [])

cloneStorage :: State UUID SSCXXAST
cloneStorage = sequence $ scxx_exprstatement $ scxx_assignment (scxx_varref "c.storage[c.head--]") "=" (scxx_literal "null")

cloneRetStatement :: State UUID SSCXXAST
cloneRetStatement = sequence $ scxx_return $ scxx_varref "c"

newReturnType :: String
newReturnType = "Stack<T>"

example :: (MonadColorPrinter m) => State UUID (ASTExample m SimpleCXXGrammar String)
example =
    do
        tree_start <- StackPopAlice.startTree
        tree_cond <- StackPopAlice.condTree
        tree_clonedef <- StackPopAlice.cloneDef
        tree_clonestorage <- StackPopAlice.cloneStorage
        tree_cloneretstatement <- StackPopAlice.cloneRetStatement
        let
            id_tree_start_body = uuidOf . fromJust $ findByGrammarType SCXX_Statements tree_start
            id_tree_start_storage = uuidOf . fromJust $ findByGrammarType SCXX_ExprStatement tree_start
            id_tree_cond_body = uuidOf . fromJust $ findByGrammarType SCXX_Statements tree_cond
            id_tree_start_ret = uuidOf . fromJust $ findByGrammarType SCXX_Type tree_start
        return Example {
            Example.name = "Motivating Example: Alice works on Stack.pop",
            Example.colours = featurecolours,
            Example.startVersion = (emptyTrace, tree_start),
            history = zip [ -- Edits
                edit_ins_tree tree_cond id_tree_start_body 0
              , edit_move_tree id_tree_start_storage id_tree_cond_body 0
              , edit_del_tree id_tree_start_storage
              , edit_ins_tree tree_clonestorage id_tree_cond_body 0
              , edit_ins_tree tree_clonedef id_tree_start_body 0
              , edit_ins_tree tree_cloneretstatement id_tree_start_body 2
              , edit_update id_tree_start_ret SCXX_Type newReturnType
            ] [ -- corresponding feature contexts
                Just $ PVariable feature_SafeStack
              , Nothing
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
              , Just $ PVariable feature_ImmutableStack
            ]
        }