module Logic where

class Logic l where
    ltrue :: l
    lfalse :: l
    lvalues :: [l]

    lnot :: l -> l
    lnot q = limplies q lfalse
    land :: l -> l -> l
    land p q = lnot $ lor (lnot p) (lnot q)
    lor :: l -> l -> l
    -- lor p q = limplies (limplies p q) q
    lor p q = lnot $ land (lnot p) (lnot q)
    limplies :: l -> l -> l
    limplies p q = lor (lnot p) q
    lequals :: l -> l -> l
    lequals p q = land (limplies p q) (limplies q p)

    leval :: (l -> l) -> l -> l