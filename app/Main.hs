module Main where

import Control.Monad.State

-- import Prelude hiding (putStr)
-- import Data.ByteString.Char8 (putStr)
-- import Data.ByteString.UTF8 (fromString)

import UUID
import Util
import Tree
import AST
import Edits
import FeatureTrace
import FeatureTraceRecording
import Propositions
import NullPropositions
import SAT

import Div

import Data.Maybe
import Data.List (intercalate)

main :: IO ()
main = putStrLn . fst . flip runState 0 $ do
    -- Unpack the states i.e. apply >>= in a more convenient way
    tree0 <- div0
    tree_assert <- div_assert
    tree_error <- div_error
    tree_condition <- div_condition
    tree_div <- div_div
    let 
        -- Debug settings
        showPC = False
        abstractTrees = True
        -- The initial feature trace of the first tree.
        trace0 = emptyTrace
        -- Some helper variables for edits
        id_div_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree0
        id_div_cond_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree_condition
        -- The edits "made by the developer"
        editscript = [
            edit_ins_tree tree_assert id_div_body 0
          , edit_ins_partial tree_condition id_div_body 0 0 id_div_cond_body 0
          , edit_del_tree (uuidOf tree_assert)
          , edit_ins_tree tree_error id_div_cond_body 0
          , edit_ins_tree tree_div (uuidOf tree0) 0
          -- , edit_move_tree 
            ]
        -- The feature contexts assigned to each edit
        featureContexts = [
            Just $ PVariable "Debug"
          , Just $ PVariable "Reciprocal"
          , Just $ PVariable "Reciprocal" -- Error by user. Should actually be PTrue
          , Just $ PVariable "Reciprocal"
          , Just $ PVariable "Division"
          -- , Just $ PVariable "Division"
            ]
        -- (finalTrace, finalTree) = featureTraceRecording trace0 tree0 editscript featureContexts
        -- Run the ftr
        tracesAndTrees = featureTraceRecordingWithIntermediateSteps trace0 tree0 editscript featureContexts
        -- Some helper variables for output formatting
        featureFormulaOutputFormatter = \trace tree -> if showPC then pc tree trace else trace
        treeOutputFormatter = if abstractTrees then abstract else id
    --   return $ show $ showTrace finalTrace finalTree
    return
      $ foldr (\(fc, edit, tree) s ->
        "\n==== Run "
        ++(show edit)
        ++" under context="
        ++(NullPropositions.prettyPrint fc)
        ++" giving us ====\n"
        ++(FeatureTrace.prettyPrint tree)
        ++s) ""
      $ zip3
          ((Just $ PAnd [PVariable "meta_Debug", PVariable "meta_DoesNotMatterAtAll"]):featureContexts) -- Prepend dummy feature context here as fc for initial tree
          (edit_identity:editscript) -- Prepend identity edit here to show initial tree.
          ((uncurry augmentWithTrace).(\(trace, tree) -> (featureFormulaOutputFormatter trace tree, treeOutputFormatter tree)) <$> ((trace0, tree0):tracesAndTrees))
