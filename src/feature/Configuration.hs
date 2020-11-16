module Configuration where

import Data.Map
import PropositionalFormula

type Configuration a = Map a Bool

satisfies :: Configuration a -> PropositionalFormula a -> Bool
satisfies config = eval (config!)

isTotal :: Set a -> Configuration a -> Bool
isTotal variables config = (keysSet config) isSubsetOf variables

isPartial :: Set a -> Configuration a -> Bool
isPartial = not.isTotal