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

import Data.Maybe
import Data.List (intercalate)

absAST :: State UUID (AST String)
absAST = sequence
    (Tree (node "abs" ASTT_FuncDef) [
        (Tree (node "params" ASTT_Parameters) [
            (Tree (node "param" ASTT_VarDecl) [
                (Tree (node "int" ASTT_Type) []),
                (Tree (node "x" ASTT_Literal) [])
            ])
        ]),
        (Tree (node "body" ASTT_Statements) [
            (Tree (node "if" ASTT_Condition) [
                (Tree (node "cond" ASTT_Expression) [
                    (Tree (node "<" ASTT_BinaryOp) [
                        (Tree (node "x" ASTT_VarRef) []),
                        (Tree (node "0" ASTT_Literal) [])
                        -- Should these really be plain here?
                        -- They cant be removed because they belong to the definition of < which needs a left and right side.
                        -- However, they could be replaced in other variants.
                    ])
                ]),
                (Tree (node "Statements" ASTT_Statements) [
                    (Tree (node "return" ASTT_Return) [
                        (Tree (node "Expression" ASTT_Expression) [
                            (Tree (node "UnaryMinus" ASTT_UnaryOp) [
                                (Tree (node "x" ASTT_VarRef) [])
                            ])
                        ])
                    ])
                ])
            ]),
            (Tree (node "return" ASTT_Return) [
                (Tree (node "Expression" ASTT_Expression) [
                    (Tree (node "x" ASTT_VarRef) [])
                ])
            ])
        ])
    ])

assertAST :: State UUID (AST String)
assertAST = sequence
    (Tree (node "Assertion" ASTT_FuncCall) [
        (Tree (node "assert_expr" ASTT_Expression) [
            (Tree (node "!=" ASTT_BinaryOp) [
                (Tree (node "x" ASTT_VarRef) []),
                (Tree (node "NaN" ASTT_Literal) [])
            ])
        ])
    ])

main :: IO ()
-- main = putStrLn . show $ runState absAST 0
-- main = putStrLn . show $ runState (assertAST >>= \smallTree -> absAST >>= \bigTree -> return $ edit (InsTree {tree = smallTree, pos = uuidOf $ crack $ find bigTree (\(Tree n _) -> value n == "Statements"), index = 0}) bigTree) 0

main = putStrLn . show . fst . flip runState 0 $ do
  -- Unpack the states i.e. apply >>= in a more convenient way
  tree0 <- absAST
  treeToInsert <- assertAST
  let 
      -- The initial feature trace of the first tree.
      trace0 = emptyTrace
      -- The edits "made by the developer"
      editscript = [
           edit_ins_tree treeToInsert (uuidOf . fromJust $ find tree0 (\(Tree n _) -> value n == "Statements")) 0
        --   ,edit_del_tree (uuidOf treeToInsert)
          ]
      -- The feature contexts assigned to each edit
      featureContexts = [
          Just $ PVariable "SecureNumbers"
        --   , Nothing
        ]
      (finalTrace, finalTree) = featureTraceRecording trace0 tree0 editscript featureContexts
  -- Run the feature trace recording
  return $ showTrace finalTrace finalTree
--   return $ abstract tree0
--   return $ showTrace (newTrace 15 2 (Just $ PAnd [PVariable "A", PVariable "B"])) $ foldEditScript editscript tree0
--   return $ intercalate ", " (fmap name editscript)

