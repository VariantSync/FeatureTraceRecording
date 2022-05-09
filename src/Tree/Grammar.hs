{- |
Description: Type class for context-free grammars.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Type class for context-free grammars.
-}
module Tree.Grammar where

-- | Classification of nodes for feature traces and presence condition.
-- (See Section 3.1 in the paper.)
data NodeType = Mandatory | Optional | Wrapper deriving (Show, Eq)

-- | Type class for context-free grammars.
class Show g => Grammar g where
  -- | Tells for each grammar symbol which node type it has (e.g., if it is 'Mandatory' in the AST).
  nodetypeof :: g -> NodeType
