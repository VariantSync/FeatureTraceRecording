module FeatureTrace where

import AST

-- data PropFormula = True | False | Null | Variable x | Not PropFormula | And PropFormula PropFormula | Or PropFormula PropFormula
-- data FeatureTrace :: AST a -> PropFormula

-- data PresenceCondition :: FeatureTrace -> AST a -> PropFormula
-- PresenceCondition f t = And (f t) (
--   case parent t of
--     Nothing -> PropFormula.True
--     Just p -> case ntype p of
--       Legator -> PresenceCondition f p
--       Plain -> PresenceCondition f p
--