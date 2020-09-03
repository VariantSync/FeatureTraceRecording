import Propositions
import SAT
import Util
import Simplify

-- testImpl :: PropositionalFormula String
-- testImpl = pimplies (PAnd [PVariable "A", PVariable "B"]) (PVariable "C")

-- testDNF :: PropositionalFormula String
-- testDNF = POr [PAnd [PVariable "A", PVariable "B"], PAnd [PVariable "C", PNot (POr [PVariable "A", PVariable "B"])]]

-- testNot :: PropositionalFormula String
-- testNot = PNot testImpl

-- testCNF :: PropositionalFormula String
-- testCNF = PAnd [POr [PVariable "A", PVariable "B"], POr [PNot (PVariable "A"), PVariable "D"]]

-- testTrue :: PropositionalFormula String
-- testTrue = PAnd [PTrue, POr [PFalse, pimplies PFalse PTrue]]

-- testFalse :: PropositionalFormula String
-- testFalse = PAnd [PAnd [PFalse, PTrue], POr [PFalse, pimplies PFalse PTrue]]

-- {-Test CNF conversion-}
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

testCases :: [(PropositionalFormula [Char], PropositionalFormula [Char])]
testCases = [
    (PAnd [PVariable "A", PVariable "B"], PVariable "A"),
    (PVariable "A", PAnd [PVariable "A", PVariable "B"]),
    (PVariable "A", PAnd [PVariable "B", PVariable "A"]),
    (PVariable "A", PAnd [POr [PVariable "X", PVariable "A"], PVariable "B"]),
    (PAnd [PVariable "A", PVariable "B"], PAnd [PVariable "B", PVariable "A"]),
    (POr [PVariable "A", PVariable "B"], PAnd [POr [PVariable "B", PVariable "A"], PVariable "X"]),
    (POr [PVariable "A", PVariable "B"], PAnd [PVariable "X", POr [PVariable "B", PVariable "A"]]),
    (PVariable "A", POr [PVariable "A", PVariable "B"]),
    (POr [PVariable "A", PVariable "B"], PVariable "A"),
    (PNot $ PVariable "A", POr [PVariable "A", PVariable "B"]),
    (PAnd [PNot $ PVariable "A", PVariable "X"], POr [PVariable "A", PVariable "B"]),
    (PNot $ PVariable "A", POr [PAnd [PVariable "A", PVariable "X"], PVariable "B"]),
    (PNot $ PVariable "A", PVariable "A"),
    (POr [PVariable "A", PVariable "B"], POr [PVariable "B", PVariable "A"]),
    (PVariable "A", POr [PVariable "B", PVariable "A"]),
    (PNot $ PVariable "A", PAnd [PVariable "B", PVariable "A"]),
    (PAnd [PNot $ PVariable "A", PVariable "A"], PVariable "X")
    ]

main :: IO ()
main = putStrLn $
    foldr (\a b -> a++"\n"++b) mempty $
    map (\(axiom, formula) -> concat [
        "             axiom = ", show axiom, "\n",
        "           formula = ", show formula, "\n",
        "simplified formula = ", show $ removeRedundancy axiom formula, "\n"]) testCases