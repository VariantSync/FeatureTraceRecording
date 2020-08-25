module UUID where

import Control.Monad.State

type UUID = Int

next :: () -> State UUID ()
next x = do
    num <- get
    put (num + 1)
    return x

toInt :: UUID -> Int
toInt = id

epsilon :: UUID
epsilon = -1