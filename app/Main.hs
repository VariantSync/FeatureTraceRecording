module Main where

import Control.Monad.State

import Util
import Tree
import FTRNode
import Edits
import Data.List (intercalate)

absAST :: State UUID (Tree (Node String))
absAST = sequence
    (Tree (newNode "abs" Legator) [
        (Tree (newNode "Parameters" Plain) [
            (Tree (newNode "VarDecl" Legator) [
                (Tree (newNode "int" Constituent) []),
                (Tree (newNode "x" Constituent) [])
            ])
        ]),
        (Tree (newNode "Statements" Plain) [
            (Tree (newNode "if" Constituent) [
                (Tree (newNode "Expression" Plain) [
                    (Tree (newNode "<" Constituent) [
                        (Tree (newNode "x" Plain) []),
                        (Tree (newNode "0" Plain) [])
                        -- Should these really be plain here?
                        -- They cant be removed because they belong to the definition of < which needs a left and right side.
                        -- However, they could be replaced in other variants.
                    ])
                ]),
                (Tree (newNode "Statements" Legator) [
                    (Tree (newNode "return" Legator) [
                        (Tree (newNode "Expression" Plain) [
                            (Tree (newNode "UnaryMinus" Constituent) [
                                (Tree (newNode "x" Constituent) [])
                            ])
                        ])
                    ])
                ])
            ]),
            (Tree (newNode "return" Legator) [
                (Tree (newNode "Expression" Plain) [
                    (Tree (newNode "x" Constituent) [])
                ])
            ])
        ])
    ])

assertAST :: State UUID (Tree (Node String))
assertAST = sequence -- fix types
    (Tree (newNode "Assertion" Legator) [
        (Tree (newNode "Expression" Plain) [
            (Tree (newNode "!=" Legator) [
                (Tree (newNode "x" Constituent) []),
                (Tree (newNode "NaN" Constituent) [])
            ])
        ])
    ])

main :: IO ()
-- main = putStrLn . show $ runState absAST 0
-- main = putStrLn . show $ runState (assertAST >>= \smallTree -> absAST >>= \bigTree -> return $ edit (InsTree {tree = smallTree, pos = uuidOf $ crack $ find bigTree (\(Tree n _) -> value n == "Statements"), index = 0}) bigTree) 0

main = putStrLn . show . flip runState 0 $ do
  -- Unpack the states i.e. apply >>= in a more convenient way
  tree0 <- absAST
  treeToInsert <- assertAST
  -- define some helper variables that make things easier to read
  let p = crack $ find tree0 (\(Tree n _) -> value n == "Statements")
      editscript = [
           ins_tree treeToInsert (uuidOf p) 0
          ,del_tree (uuidOf treeToInsert)
          ]
  -- do the actual functionality: Therefore, we fold the edit script into one big edit
--   return $ tree0
--   return $ abstract tree0
  return $ (reversefoldr (.) id (fmap run editscript)) tree0
--   return $ intercalate ", " (fmap name editscript)

-- GHCI COMMANDS
  -- go to ghci with: stack ghci
  -- In ghci:
  --   to get info: ":i NAME"
  --   to exit: ":q"
  --   to reload: ":r"
  --   autobuild: "stack build --file-watch"
  --   autorun main: "stack build --file-watch --exec "stack run ftr""