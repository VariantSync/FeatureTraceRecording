module Logic where

class Logic l where
    ltrue :: l
    lfalse :: l
    lvalues :: [l]

    lnot :: l -> l
    lnot q = limplies q lfalse
    land :: [l] -> l
    land = lnot.lor.map lnot
    lor :: [l] -> l
    -- lor p q = limplies (limplies p q) q
    lor = lnot.land.map lnot
    limplies :: l -> l -> l
    limplies p q = lor [lnot p, q]
    lequals :: l -> l -> l
    lequals p q = land [limplies p q, limplies q p]

    leval :: (l -> l) -> l -> l