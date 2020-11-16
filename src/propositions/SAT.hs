module SAT (
    sat,
    satAssignment,
    tautCounterExample,
    taut,
    contradicts,
    toIntCNF) where

import UUID
import Propositions
import Util

import Picosat
import Control.Monad.State
import Data.Bimap
import Data.List (find)
import Data.Maybe (isJust)

-- We hack this for now. Once our system works, we can remove this and let the monads take over.
import System.IO.Unsafe (unsafePerformIO)

sat :: (Show a, Ord a) => PropositionalFormula a -> Bool
sat = isJust.satAssignment

satAssignment :: (Show a, Ord a) => PropositionalFormula a -> Maybe (Assignment a)
satAssignment p = 
    let (cnf, m) = toIntCNF p in
        unsafePerformIO $ do -- TODO: Remove this hack
            res <- solve cnf
            case res of
                Solution s -> return $ Just
                    (\x -> case find (\i -> abs i == m ! x) s of
                        Nothing -> error $ (show x)++" is not a variable in this configuration!"
                        Just i -> i >= 0)
                Unsatisfiable -> return Nothing
                Unknown -> return Nothing

taut :: (Show a, Ord a) => PropositionalFormula a -> Bool
-- taut p = sat (PNot p) >>= return . not
taut = not.sat.PNot

{-
Returns 'Nothing' if the given formula is a tautology.
Otherwise, returns a counterexample, i.e., an assignment under which the given formula evaluates to 'False'.
-}
tautCounterExample :: (Show a, Ord a) => PropositionalFormula a -> Maybe (Assignment a)
tautCounterExample = satAssignment.PNot

contradicts :: (Show a, Ord a) => PropositionalFormula a -> Bool
contradicts = not.sat

{-
Converts the given formula to a list of clauses (first argument).
Each clause is represented as a list of literals where literals are represented as integers.
Negative literals indicate negation of variables.
For example, the literal '-3' translates to 'PNot x', where x is the variable represented by '3'.
The mapping from integers to variables is returned as a bimap in the second argument.
-}
toIntCNF :: (Ord a) => PropositionalFormula a -> ([[Int]], Bimap a Int)
toIntCNF p = fst $ flip runState 1 $ do
    (m, p') <- intifyFormula empty $ simplify p
    return (clausifyCNF (\x -> -x) (\_ -> error "This is impossible.") $ lazyToCNF p', m)

intifyFormula :: (Ord a) => Bimap a Int -> PropositionalFormula a -> State UUID (Bimap a Int, PropositionalFormula Int)
intifyFormula m PTrue = do
    (m', conflict) <- createConflictingLiterals m
    return (m', POr conflict)
intifyFormula m PFalse = do
    (m', conflict) <- createConflictingLiterals m
    return (m', PAnd conflict)
intifyFormula m (PVariable v) =
    if member v m
    then return (m, PVariable (m ! v))
    else do
        next ()
        uuidForV <- get
        let intval = toInt uuidForV in
            return (insert v intval m, PVariable intval)
intifyFormula m (PNot p) = do
    (m', p') <- intifyFormula m p
    return (m', PNot p')
intifyFormula m (PAnd cs) = do
    (m', cs') <- intifyFormulas m cs
    return (m', PAnd cs')
intifyFormula m (POr cs) = do
    (m', cs') <- intifyFormulas m cs
    return (m', POr cs')

{-
Applies 'intifyFormula' to all formulas
-}
intifyFormulas :: (Ord a) => Bimap a Int -> [PropositionalFormula a] -> State UUID (Bimap a Int, [PropositionalFormula Int])
-- (m, []) is the initial state of the fold. At the beginning, the map is unchanged and no formulas have been processed.
-- foldM will fill this list of formulas one step at a time while adapting the map if necessary.
intifyFormulas m formulas = foldM intifyFormulas_fold (m, []) (reverse formulas) -- reverse is just here for human readability: The output will have the same order as the input then.

{-
The folding function for intifyFormulas.
It takes the current progress of the fold (mi, cis) and a formula to intify c:
    mi  - The current mapping from values a to ints.
    cis - A list of already intified formulas
    c   - A formula of values a that has to be intified and merged into (mi, cis).
-}
intifyFormulas_fold :: (Ord a) => (Bimap a Int, [PropositionalFormula Int]) -> PropositionalFormula a -> State UUID (Bimap a Int, [PropositionalFormula Int])
intifyFormulas_fold (mi, cis) c = do
    (mi', c') <- intifyFormula mi c
    return (mi', [c']++cis)

{-
Creates a list of the two literals 'x' and 'not x' where x is a new generated variable.
-}
createConflictingLiterals :: (Ord a) => Bimap a Int -> State UUID (Bimap a Int, [PropositionalFormula Int])
createConflictingLiterals m = do
    next ()
    z <- get
    let intval = toInt z in
        return (m, [PVariable intval, PNot $ PVariable intval])