module Propositions where

import Logic
import Data.List ( intercalate )

data PropositionalFormula a =
      PTrue
    | PFalse
    | PVariable a
    | PNot (PropositionalFormula a)
    | PAnd [PropositionalFormula a]
    | POr [PropositionalFormula a]
    deriving (Eq)

-- instance Logic (PropositionalFormula a) Bool where
--     ltrue = PTrue
--     lfalse = PFalse
--     lvalues = [lfalse, ltrue]

--     lnot PTrue = PFalse
--     lnot PFalse = PTrue
--     lnot p = PNot p
    
--     land [] = PTrue
--     land l = PAnd l

--     lor [] = PFalse
--     lor l = POr l
    
--     leval _ PTrue = True
--     leval _ PFalse = False
--     leval config (PNot x) = not $ leval config x -- This should evaluate as config should only map to lvalues and lnot directly inverts PTrue and PFalse.
--     leval config (PAnd cs) = and $ fmap (leval config) cs
--     leval config (POr cs) = or $ fmap (leval config) cs
--     leval config v@(PVariable x) = config v

instance Logic (PropositionalFormula a) where
    ltrue = PTrue
    lfalse = PFalse

    lnot PTrue = PFalse
    lnot PFalse = PTrue
    lnot p = PNot p
    
    land [] = PTrue
    land l = PAnd l

    lor [] = PFalse
    lor l = POr l
    
    leval config (PNot x) = lnot $ leval config x -- This should evaluate as config should only map to lvalues and lnot directly inverts PTrue and PFalse.
    leval config (PAnd cs) = liftBool.not $ any isPFalse $ fmap (leval config) cs
    leval config (POr cs) = liftBool $ any isPTrue $ fmap (leval config) cs
    leval config p = config p

instance Functor PropositionalFormula where
    fmap f PTrue = PTrue
    fmap f PFalse = PFalse
    fmap f (PVariable a) = PVariable (f a)
    fmap f (PNot p) = PNot (fmap f p)
    fmap f (PAnd c) = PAnd (fmap (fmap f) c)
    fmap f (POr c) = POr (fmap (fmap f) c)

-- Maybe this should better be implemented as a map as an Assignment always is a partial function.
type Assignment a = a -> Bool

eval :: Assignment a -> PropositionalFormula a -> Bool
eval _ PTrue = True
eval _ PFalse = False
eval config v@(PVariable x) = config x
eval config (PNot x) = not $ eval config x
eval config (PAnd cs) = and $ fmap (eval config) cs
eval config (POr cs) = or $ fmap (eval config) cs

liftBool :: Bool -> PropositionalFormula a
liftBool True = PTrue
liftBool False = PFalse

isPTrue :: PropositionalFormula a -> Bool
isPTrue PTrue = True
isPTrue _ = False

isPFalse :: PropositionalFormula a -> Bool
isPFalse PFalse = True
isPFalse _ = False

isLiteral :: PropositionalFormula a -> Bool
isLiteral PTrue = True
isLiteral PFalse = True
isLiteral (PVariable x) = True
isLiteral (PNot f) = isLiteral f
isLiteral _ = False

isCNF :: PropositionalFormula a -> Bool
isCNF (PAnd cs) = all isCNF cs
isCNF (POr cs) = all isLiteral cs
isCNF p = isLiteral p

toCNF :: PropositionalFormula a -> PropositionalFormula a
toCNF n@(PNot a) = case a of
    PTrue -> PFalse
    PFalse -> PTrue
    (PVariable x) -> n
    (PNot x) -> toCNF x
    (PAnd cs) -> toCNF $ POr $ fmap lnot cs
    (POr cs) -> toCNF $ PAnd $ fmap lnot cs
toCNF a@(PAnd cs) = simplify $ PAnd $ fmap toCNF cs
toCNF o@(POr cs) = simplify $ PAnd $ foldr cartesianOr [PFalse] $ fmap toCNFClauseList $ fmap toCNF cs -- TODO
toCNF p = p

lazyToCNF :: PropositionalFormula a -> PropositionalFormula a
lazyToCNF p
    | isCNF p   = p
    | otherwise = toCNF p

{-
Assumes that the given formula is in CNF.
Gives a list of the CNF's clauses.
The original formula can be reconstructed with PAnd (toClauseList x).
-}
toCNFClauseList :: PropositionalFormula a -> [PropositionalFormula a]
toCNFClauseList (PAnd cs') = cs'
toCNFClauseList p = [p]

toDNFClauseList :: PropositionalFormula a -> [PropositionalFormula a]
toDNFClauseList (POr cs') = cs'
toDNFClauseList p = [p]

toNNF :: PropositionalFormula a -> PropositionalFormula a
toNNF (PNot (POr cs)) = PAnd $ toNNF.PNot <$> cs
toNNF (PNot (PAnd cs)) = POr $ toNNF.PNot <$> cs
toNNF (PNot (PNot x)) = toNNF x
toNNF (POr cs) = POr $ toNNF <$> cs
toNNF (PAnd cs) = PAnd $ toNNF <$> cs
toNNF p = p

{-
Returns a list of disjunctions such that all possible combinations of the formulas in the input lists are considered.
Like the cartesian product but instead of pairs (x, y) we produce POr [x, y].
-}
cartesianOr :: [PropositionalFormula a] -> [PropositionalFormula a] -> [PropositionalFormula a]
cartesianOr l1 l2 = [POr [x, y] | x <- l1, y <- l2]

simplify :: PropositionalFormula a -> PropositionalFormula a
simplify (PNot a) = case simplify a of
    PTrue -> PFalse
    PFalse -> PTrue
    (PNot x) -> x
    p -> PNot p
simplify (PAnd cs) = case
    filter (not . isPTrue) -- Filter all Trues
    $ concat $ fmap (toCNFClauseList . simplify) cs -- simplify all children and flatten the formula
    of
        [] -> PTrue
        [p] -> p
        clauses -> if any isPFalse clauses -- If any value in a disjunction is false, the disjunction becomes false.
                   then PFalse
                   else PAnd clauses
simplify (POr cs) = case
    filter (not . isPFalse) -- Filter all Falses
    $ concat $ fmap (toDNFClauseList . simplify) cs -- simplify all children and flatten the formula
    of
        [] -> PFalse
        [p] -> p
        clauses -> if any isPTrue clauses -- If any value in a disjunction is true, the disjunction becomes true.
                   then PTrue
                   else POr clauses
simplify p = p

{-
Assumes that the given formula is in CNF.
The first argument is a functions resolving negation, i.e. it should take an 'a' and give its negated version.
The second argument is a function creating the value of type a that should be associated with 'false' (PFalse).
For example, 0 could represent False in propositional formulas over numerals.
(Note, if no such value exists, the 'false' function is free to produce an error.)
The third argument is the function in CNF that should be clausified.
If still confused, try the following call and look at the result:
    show $ clausifyCNF (\x -> "not "++x) "false" (toCNF p)
where p is a propositional formula over strings.
-}
clausifyCNF :: (Show a) => (a -> a) -> (() -> a) -> PropositionalFormula a -> [[a]]
clausifyCNF _ _ PTrue  = [[]]
clausifyCNF _ false PFalse = [[false ()]] --error "PFalse cannot be clausifed."
clausifyCNF _ _ (PVariable v) = [[v]]
clausifyCNF negator _ n@(PNot p) = case p of
    PVariable v -> [[negator v]]
    _ -> error $ "Given formula "++show n++" not in CNF!"
clausifyCNF negator false and@(PAnd clauses) = if isCNF and
    then concat $ fmap (clausifyCNF negator false) clauses
    else error $ "Given formula "++show and++" not in CNF!"
clausifyCNF negator false or@(POr literals) = if isCNF or
    then [concat $ concat $ fmap (clausifyCNF negator false) literals]
    else error $ "Given formula "++show or++" not in CNF!"
                            
-- -- Default ASCII expressions
-- instance Show a => Show (PropositionalFormula a) where
--     show PTrue = "T"
--     show PFalse = "F"
--     show (PVariable v) = show v
--     show (PNot p) = "¬"++show p
--     show (PAnd cs) = parenIf (length cs > 1) (intercalate " ^ " $ map show cs)
--     show (POr cs) = parenIf (length cs > 1) (intercalate " v " $ map show cs)

-- Nice UTF8 styled expressions
instance Show a => Show (PropositionalFormula a) where
    show PTrue = "⊤"
    show PFalse = "⊥"
    show (PVariable v) = show v
    show (PNot p) = "¬"++show p
    show (PAnd cs) = "("++(intercalate " ∧ " $ map show cs)++")"
    show (POr cs) = "("++(intercalate " ∨ " $ map show cs)++")"

-- -- This visualisation is for debugging as it shows the exact expression tree.
-- instance Show a => Show (PropositionalFormula a) where
--     show PTrue = "(PTrue)"
--     show PFalse = "(PFalse)"
--     show (PVariable v) = "(PVariable "++show v++")"
--     show (PNot p) = "(PNot "++show p++")"
--     show (PAnd cs) = "(PAnd ["++(intercalate ", " $ map show cs)++"])"
--     show (POr cs) = "(POr ["++(intercalate ", " $ map show cs)++"])"
