module SAT where

import Propositions
import Picosat

sat :: PropositionalFormula a -> Bool
sat p = False

taut :: PropositionalFormula a -> Bool
taut p = not $ sat $ PNot p