-- | Module representing universable unique identifiers (UUID).
module UUID where

import Control.Monad.State

-- | A universable unique identifiers (UUID) realised as an integer.
type UUID = Int

-- | Computes successor UUID of the current UUID.
next :: State UUID ()
next = do
    num <- get
    put (num + 1)
    return ()

-- | Converts a UUID to an int.
toInt :: UUID -> Int
toInt = id

-- | An invalid value for UUID that can be used to represent invalid states.
epsilon :: UUID
epsilon = -1