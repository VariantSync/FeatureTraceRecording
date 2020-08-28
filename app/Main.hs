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
import SAT

import Div

import Data.Maybe
import Data.List (intercalate)

main :: IO ()
main = putStrLn . show . fst . flip runState 0 $ do
    -- Unpack the states i.e. apply >>= in a more convenient way
    tree0 <- div0
    assert <- div_assert
    condition <- div_condition
    let 
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
        (finalTrace, finalTree) = featureTraceRecording trace0 tree0 editscript featureContexts
    -- Run the feature trace recording
    return $ showTrace finalTrace finalTree
    --   return $ abstract tree0
    --   return $ showTrace (newTrace 15 2 (Just $ PAnd [PVariable "A", PVariable "B"])) $ foldEditScript editscript tree0
    --   return $ intercalate ", " (fmap name editscript)

