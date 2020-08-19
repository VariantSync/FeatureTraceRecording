module SAT where

import Propositions
import Picosat

toPicosatFormat :: PropositionalFormula a -> [[Int]]
toPicosatFormat p = [[1]] --(map toDNFClauseList $ toCNFClauseList $ lazyToCNF p)

sat :: PropositionalFormula a -> IO Bool
sat p = do
    res <- solve $ toPicosatFormat p
    case res of
        Solution _ -> return True
        Unsatisfiable -> return False
        Unknown -> return False

taut :: PropositionalFormula a -> IO Bool
taut p = sat (PNot p) >>= \s -> return $ not s