{- |
Description: Module representing universable unique identifiers ('UUID').
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module representing universable unique identifiers ('UUID').
We use these to identify nodes in abstract syntax trees ('AST') and track them across versions.
-}
module UUID where

import Control.Monad.State

-- | A universable unique identifiers (UUID) realised as an integer.
type UUID = Int

-- | Computes the successor 'UUID' of the current 'UUID'.
next :: State UUID UUID
next = do
    modify (+ 1)
    get

-- | Converts a 'UUID' to an 'Int'.
toInt :: UUID -> Int
toInt = id