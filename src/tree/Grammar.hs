-- | Module for representing context-free grammars.
module Grammar where

-- | Classification of nodes for feature traces and presence condition.
-- (See Section 3.1 in the paper.)
data NodeType = Mandatory | Optional | Wrapper deriving (Show, Eq)

-- | Type class for context-free grammars.
class Show g => Grammar g where
  -- | Tells for each grammar symbol which node type it has (e.g., if it is 'Mandatory' in the AST).
  nodetypeof :: g -> NodeType
