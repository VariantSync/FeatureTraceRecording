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

data CodePrintStyle = ShowAST | ShowCode
data TraceStyle = ShowTrace | ShowPresenceCondition | ShowNone deriving Eq

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
        codeStyle = ShowAST
        traceStyle = ShowTrace
        abstractTrees = False
        -- The initial feature trace of the first tree.
        trace0 = emptyTrace
        -- Some helper variables for edits
        id_reciprocal_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree0
        id_return = uuidOf . fromJust $ find (\(Tree n _) -> valuetype n == ASTT_Return) tree0
        id_cond_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree_condition
        id_div_body = uuidOf . fromJust $ find (\(Tree n _) -> value n == "body") tree_div
        -- The edits "made by the developer"
        editscript = [
            edit_ins_tree tree_assert id_reciprocal_body 0
          , edit_ins_partial tree_condition id_reciprocal_body 0 0 id_cond_body 0
          , edit_del_tree (uuidOf tree_assert)
          , edit_ins_tree tree_error id_cond_body 0
          , edit_ins_tree tree_div (uuidOf tree0) 0
          , edit_move_tree (uuidOf tree_condition) id_div_body 0
          , edit_move_tree id_return id_div_body 1 -- now we are done with div5.txt
            ]
        -- The feature contexts assigned to each edit
        featureContexts = [
            Just $ PVariable "Debug"
          , Just $ PVariable "Reciprocal"
          , Just $ PVariable "Reciprocal" -- Error by user. Should actually be PTrue
          , Just $ PVariable "Reciprocal"
          , Just $ PVariable "Division"
          , Just $ PVariable "Division"
          , Just $ PVariable "Division"
            ]
        -- (finalTrace, finalTree) = featureTraceRecording trace0 tree0 editscript featureContexts
        -- Run the ftr
        tracesAndTrees = featureTraceRecordingWithIntermediateSteps trace0 tree0 editscript featureContexts
        -- Some helper variables for output formatting
        toPC = \trace tree -> if traceStyle == ShowPresenceCondition then pc tree trace else trace
        treeAbstract = if abstractTrees then abstract else id
        treePrint = case codeStyle of
          ShowAST -> \tree trace -> if traceStyle /= ShowNone then FeatureTrace.prettyPrint $ augmentWithTrace trace tree else show tree
          ShowCode -> \tree trace -> showContent 0 (\n -> showIt trace n) tree
            where showIt trace n = if traceStyle /= ShowNone then concat ["<", NullPropositions.prettyPrint $ trace n, ">", value n] else value n
    --   return $ show $ showTrace finalTrace finalTree
    return
      $ flip foldr mempty (\(fc, edit, (trace, tree)) s ->
        concat [
        "\n==== Run ", (show edit), " under context = ", (NullPropositions.prettyPrint fc), " giving us ====\n"
        -- ,(FeatureTrace.prettyPrint tree)
        , treePrint tree trace
        , s])
      $ zip3
          ((Just $ PAnd [PVariable "meta_Debug", PVariable "meta_DoesNotMatterAtAll"]):featureContexts) -- Prepend dummy feature context here as fc for initial tree
          (edit_identity:editscript) -- Prepend identity edit here to show initial tree.
          ((\(trace, tree) -> (toPC trace tree, treeAbstract tree)) <$> ((trace0, tree0):tracesAndTrees))
