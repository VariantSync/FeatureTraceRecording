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
    assert <- div_assert
    condition <- div_condition
    let 
        -- Output settings
        showPC = False
        -- The initial feature trace of the first tree.
        trace0 = emptyTrace
        -- Some helper variables for edits
        id_div_body = uuidOf . fromJust $ find tree0 (\(Tree n _) -> value n == "body")
        -- The edits "made by the developer"
        editscript = [
            edit_ins_tree assert id_div_body 0
          , edit_ins_partial condition id_div_body 0 0 (uuidOf . fromJust $ find condition (\(Tree n _) -> value n == "body")) 0
            ]
        -- The feature contexts assigned to each edit
        featureContexts = [
            Just $ PVariable "Debug"
          , Just $ PVariable "Reciprocal"
            ]
        -- (finalTrace, finalTree) = featureTraceRecording trace0 tree0 editscript featureContexts
        tracesAndTrees = featureTraceRecordingWithIntermediateSteps trace0 tree0 editscript featureContexts
    --   return $ show $ showTrace finalTrace finalTree
    return
      $ (++) "\n==== Initial State ====\n"
      $ (++) (FeatureTrace.prettyPrint $ augmentWithTrace trace0 tree0)
      $ foldr (\(fc, edit, tree) s ->
        "\n==== Run "
        ++(show edit)
        ++" under context="
        ++(NullPropositions.prettyPrint fc)
        ++" giving us ====\n"
        ++(FeatureTrace.prettyPrint tree)
        ++s) ""
      $ zip3 featureContexts editscript (uncurry augmentWithTrace <$> (if showPC then fmap (\(trace, tree) -> (pc tree trace, tree)) else id) tracesAndTrees)
    --   return $ abstract tree0
    --   return $ showTrace (newTrace 15 2 (Just $ PAnd [PVariable "A", PVariable "B"])) $ foldEditScript editscript tree0
    --   return $ intercalate ", " (fmap name editscript)

