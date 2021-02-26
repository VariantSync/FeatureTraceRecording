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
import SimpleJava
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

startTree :: State UUID SSJavaAST
startTree = sequence $
    sjava_methoddef "void" "pop" [] [
        sjava_exprstatement $ sjava_assignment (sjava_varref "storage[head--]") "=" (sjava_literal "null")
    ]

condTree :: State UUID SSJavaAST
condTree = sequence $ sjava_condition (sjava_unaryop "!" $ sjava_funccall "empty" []) []

cloneDef :: State UUID SSJavaAST
cloneDef = sequence $ sjava_exprstatement $ sjava_assignment (sjava_vardecl "Stack<T>" "c") "=" (sjava_funccall "clone" [])

cloneStorage :: State UUID SSJavaAST
cloneStorage = sequence $ sjava_exprstatement $ sjava_assignment (sjava_varref "c.storage[c.head--]") "=" (sjava_literal "null")

cloneRetStatement :: State UUID SSJavaAST
cloneRetStatement = sequence $ sjava_return $ sjava_varref "c"

newReturnType :: String
newReturnType = "Stack<T>"

example :: (MonadColorPrinter m) => State UUID (Example m SimpleJavaGrammar String)
example =
    do
        tree_start <- StackPopAlice.startTree
        tree_cond <- StackPopAlice.condTree
        tree_clonedef <- StackPopAlice.cloneDef
        tree_clonestorage <- StackPopAlice.cloneStorage
        tree_cloneretstatement <- StackPopAlice.cloneRetStatement
        let
            id_tree_start_body = uuidOf . fromJust $ findByGrammarType SJava_Statements tree_start
            id_tree_start_storage = uuidOf . fromJust $ findByGrammarType SJava_ExprStatement tree_start
            id_tree_cond_body = uuidOf . fromJust $ findByGrammarType SJava_Statements tree_cond
            id_tree_start_ret = uuidOf . fromJust $ findByGrammarType SJava_Type tree_start
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
              , edit_update id_tree_start_ret SJava_Type newReturnType
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