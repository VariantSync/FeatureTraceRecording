module Truthtable where

import Logic

unarytable :: Logic a => (a -> a) -> [a] -> [a]
unarytable op values = leval id . op <$> values

cartesian :: [a] -> [b] -> [(a, b)]
cartesian x y = (,) <$> x <*> y

binarytable :: Logic a => (a -> a -> a) -> [a] -> [a]
binarytable op values = leval id . uncurry op <$> cartesian values values

binarify :: ([a] -> b) -> (a -> a -> b)
binarify f = \x -> \y -> f [x, y]

hfillto :: Int -> String -> String
hfillto i s
    | missingspace > 0 = (s++) $ replicate missingspace ' '
    | otherwise = s
    where missingspace = i - length s
    
defaultlinewidth :: Int
defaultlinewidth = 7

defaulthfillto :: String -> String
defaulthfillto = hfillto defaultlinewidth

prettyunarytable :: (Logic a, Show a) => [a] -> (a -> a) -> String -> String
prettyunarytable values op name = ((name++"\n"++(replicate (2*defaultlinewidth+3) '_')++"\n")++) $ mconcat $ (\(val, res) -> unwords [defaulthfillto $ show val, "|", defaulthfillto $ show res, "\n"]) <$> zip values (unarytable op values)

prettybinarytable :: (Logic a, Show a) => [a] -> (a -> a -> a) -> String -> String
prettybinarytable values op name = ((name++"\n"++(replicate (3*defaultlinewidth+4) '_')++"\n")++) $ mconcat $ (\((a, b), res) -> unwords [defaulthfillto $ show a, defaulthfillto $ show b, "|", defaulthfillto $ show res, "\n"]) <$> zip pairs (binarytable op values)
    where pairs = cartesian values values

generatetruthtablesfor :: (Logic a, Show a) => [a] -> String
generatetruthtablesfor values =
    let
        notname = "¬"
        andname = "∧"
        orname  = "∨"
        impliesname = "⇒"
        equalsname = "⇔"
        in
    prettyunarytable values lnot notname
    ++ mconcat (("\n\n"++).uncurry (prettybinarytable values)
        <$> [
              (binarify land, andname)
            , (binarify lor, orname)
            , (limplies, impliesname)
            , (lequals, equalsname)
            ])