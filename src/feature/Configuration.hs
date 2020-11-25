module Configuration where

import Data.Map
import Data.Set
import Propositions

type Configuration a = Map a Bool

satisfies :: (Ord a) => Configuration a -> PropositionalFormula a -> Bool
satisfies config = eval (config!)

isTotal :: (Ord a) => Set a -> Configuration a -> Bool
isTotal variables config = isSubsetOf (keysSet config) variables

isPartial :: (Ord a) => Set a -> Configuration a -> Bool
isPartial variables config = not $ isTotal variables config