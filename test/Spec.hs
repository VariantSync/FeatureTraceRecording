import Propositions.Propositions
import Propositions.Simplify


{-Test CNF conversion-}

-- testImpl :: PropositionalFormula String
-- testImpl = pimplies (PAnd [varA, varB]) (PVariable "C")

-- testDNF :: PropositionalFormula String
-- testDNF = POr [PAnd [varA, varB], PAnd [PVariable "C", PNot (POr [varA, varB])]]

-- testNot :: PropositionalFormula String
-- testNot = PNot testImpl

-- testCNF :: PropositionalFormula String
-- testCNF = PAnd [POr [varA, varB], POr [PNot (varA), PVariable "D"]]

-- testTrue :: PropositionalFormula String
-- testTrue = PAnd [PTrue, POr [PFalse, pimplies PFalse PTrue]]

-- testFalse :: PropositionalFormula String
-- testFalse = PAnd [PAnd [PFalse, PTrue], POr [PFalse, pimplies PFalse PTrue]]

-- main :: IO ()
-- main = putStrLn $ 
--     foldr (\a b -> a++"\n"++b) "" $
--     map (\a -> let c = toCNF a in
--         "   Formula: "++(show a)++
--         "\n  -> isCNF: "++(show $ isCNF a)++
--         "\n       CNF: "++(show $ c)++
--         "\n  -> isCNF: "++(show $ isCNF $ c)++
--         "\nclausified: "++(show $ clausifyCNF (\s -> "not "++s) (\() -> "False") c)++
--         "\nin picosat: "++(show $ toIntCNF a)++
--         "\n       sat: "++(show $ sat a)++
--         "\n=====\n")
--     [testImpl, testDNF, testNot, testCNF, testTrue, testFalse]

-- -- main = putStrLn . show $ getRange 2 4 ['a'..'f']

{-Test PC simplification-}

varA :: PropositionalFormula String
varA = PVariable "A"
varB :: PropositionalFormula String
varB = PVariable "B"
varX :: PropositionalFormula String
varX = PVariable "X"

testCases :: [(PropositionalFormula [Char], PropositionalFormula [Char])]
testCases = [
    (PAnd [varA, varB], varA),
    (varA, PAnd [varA, varB]),
    (varA, PAnd [varB, varA]),
    (varA, PAnd [POr [varX, varA], varB]),
    (PAnd [varA, varB], PAnd [varB, varA]),
    (POr [varA, varB], PAnd [POr [varB, varA], varX]),
    (POr [varA, varB], PAnd [varX, POr [varB, varA]]),
    (varA, POr [varA, varB]),
    (POr [varA, varB], varA),
    (PNot $ varA, POr [varA, varB]),
    (PAnd [PNot $ varA, varX], POr [varA, varB]),
    (PNot $ varA, POr [PAnd [varA, varX], varB]),
    (PNot $ varA, varA),
    (POr [varA, varB], POr [varB, varA]),
    (varA, POr [varB, varA]),
    (PNot $ varA, PAnd [varB, varA]),
    (PAnd [PNot $ varA, varA], varX),
    (PTrue, varA),
    (PTrue, PNot $ PAnd [varA, varB]),
    (PTrue, PAnd [varA, varB]),
    (PTrue, PAnd [varA, PNot varB]),
    (PTrue, PAnd [varA, PAnd [varA, varB]]), -- buggy case
    (PTrue, PAnd [varA, PNot $ PAnd [varA, varB]]), -- buggy case
    (varA, PAnd [varA, PNot $ PAnd [varA, varB]])
    ]

main :: IO ()
main = putStrLn $
    foldr (\a b -> a++"\n"++b) mempty $
    map (\(axiom, formula) -> concat [
        "             axiom = ", show axiom, "\n",
        "           formula = ", show formula, "\n",
        "simplified formula = ", show $ removeRedundancy axiom (toNNF formula), "\n"]) testCases

{-Test NNF conversion-}
-- main :: IO ()
-- main = putStrLn $
--     foldr (\a b -> a++"\n"++b) mempty $
--     (\formula -> concat [
--         "formula = ", show formula, "\n",
--         "    NNF = ", show.toNNF $ formula, "\n"]) <$>
--     [
--         PNot $ PAnd [varA, varB],
--         PNot $ PNot varA,
--         PNot $ PAnd [varA, PNot varB, PNot $ POr [varX, PNot varB]]
--     ]
