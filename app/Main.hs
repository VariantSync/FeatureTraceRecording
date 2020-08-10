module Main where

import Control.Monad.State

import Util
import AST
import FTRNode
import Edits

absAST :: State UUID (AST (Node String))
absAST = sequence
    (AST (newNode "abs" Legator) [
        (AST (newNode "Parameters" Plain) [
            (AST (newNode "VarDecl" Legator) [
                (AST (newNode "int" Constituent) []),
                (AST (newNode "x" Constituent) [])
            ])
        ]),
        (AST (newNode "Statements" Plain) [
            (AST (newNode "if" Constituent) [
                (AST (newNode "Expression" Plain) [
                    (AST (newNode "<" Constituent) [
                        (AST (newNode "x" Plain) []),
                        (AST (newNode "0" Plain) [])
                        -- Should these really be plain here?
                        -- They cant be removed because they belong to the definition of < which needs a left and right side.
                        -- However, they could be replaced in other variants.
                    ])
                ]),
                (AST (newNode "Statements" Legator) [
                    (AST (newNode "return" Legator) [
                        (AST (newNode "Expression" Plain) [
                            (AST (newNode "UnaryMinus" Constituent) [
                                (AST (newNode "x" Constituent) [])
                            ])
                        ])
                    ])
                ])
            ]),
            (AST (newNode "return" Legator) [
                (AST (newNode "Expression" Plain) [
                    (AST (newNode "x" Constituent) [])
                ])
            ])
        ])
    ])

assertAST :: State UUID (AST (Node String))
assertAST = sequence -- fix types
    (AST (newNode "Assertion" Legator) [
        (AST (newNode "Expression" Plain) [
            (AST (newNode "!=" Legator) [
                (AST (newNode "x" Constituent) []),
                (AST (newNode "NaN" Constituent) [])
            ])
        ])
    ])

main :: IO ()
-- main = putStrLn . show $ runState absAST 0
-- main = putStrLn . show $ runState (assertAST >>= \smallTree -> absAST >>= \bigTree -> return $ edit (InsTree {tree = smallTree, pos = uuidOf $ crack $ find bigTree (\(AST n _) -> value n == "Statements"), index = 0}) bigTree) 0

main = putStrLn . show . flip runState 0 $ do
  -- Unpack the states i.e. apply >>= in a more convenient way
  treeToInsertTo <- absAST
  treeToInsert <- assertAST
  -- define some helper variables that make things easier to read
  let p = find treeToInsertTo (\(AST n _) -> value n == "Statements")
      e = InsTree {treeToInsert = treeToInsert, pos = uuidOf $ crack p, index = 0}
  -- do the actual functionality
  return (edit e treeToInsertTo)

  -- go to ghci with: stack ghci
  -- In ghci:
  --   to get info: ":i NAME"
  --   to exit: ":q"
  --   to reload: ":r"
  --   autobuild: "stack build --file-watch"
  --   autorun main: "stack build --file-watch --exec "stack run ftr""