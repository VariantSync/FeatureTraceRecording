import Propositions
import SAT

testImpl :: PropositionalFormula String
testImpl = pimplies (PAnd [PVariable "A", PVariable "B"]) (PVariable "C")

testDNF :: PropositionalFormula String
testDNF = POr [PAnd [PVariable "A", PVariable "B"], PAnd [PVariable "C", PNot (POr [PVariable "A", PVariable "B"])]]

testNot :: PropositionalFormula String
testNot = PNot testImpl

testCNF :: PropositionalFormula String
testCNF = PAnd [POr [PVariable "A", PVariable "B"], POr [PNot (PVariable "A"), PVariable "D"]]

testTrue :: PropositionalFormula String
testTrue = PAnd [PTrue, POr [PFalse, pimplies PFalse PTrue]]

testFalse :: PropositionalFormula String
testFalse = PAnd [PAnd [PFalse, PTrue], POr [PFalse, pimplies PFalse PTrue]]

{-Test CNF conversion-}
main = putStrLn $ 
    foldr (\a b -> a++"\n"++b) "" $
    map (\a -> let c = toCNF a in
        "   Formula: "++(show a)++
        "\n  -> isCNF: "++(show $ isCNF a)++
        "\n       CNF: "++(show $ c)++
        "\n  -> isCNF: "++(show $ isCNF $ c)++
        "\nclausified: "++(show $ clausifyCNF (\s -> "not "++s) (\() -> "False") c)++
        "\nin picosat: "++(show $ toIntCNF a)++
        "\n       sat: "++(show $ sat a)++
        "\n=====\n")
    [testImpl, testDNF, testNot, testCNF, testTrue, testFalse]