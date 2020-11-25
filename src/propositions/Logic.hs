-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Logic where

class Logic l where
    ltrue :: l
    lfalse :: l
    lvalues :: [l]
    lvalues = [lfalse, ltrue]

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

-- class Logic symbol value where
--     ltrue :: value
--     lfalse :: value
--     lvalues :: [value]
--     lvalues = [lfalse, ltrue]

--     lnot :: symbol -> symbol
--     lnot q = limplies q lfalse
--     land :: [symbol] -> symbol
--     land = lnot.lor.map lnot
--     lor :: [symbol] -> symbol
--     -- lor p q = limplies (limplies p q) q
--     lor = lnot.land.map lnot
--     limplies :: symbol -> symbol -> symbol
--     limplies p q = lor [lnot p, q]
--     lequals :: symbol -> symbol -> symbol
--     lequals p q = land [limplies p q, limplies q p]

--     leval :: (symbol -> value) -> symbol -> value

