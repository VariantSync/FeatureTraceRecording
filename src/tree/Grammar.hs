module Grammar where

{-
Classification of nodes for Feature Traces and Presence Condition
There should be a unique mapping ASTTypeAlphabet -> NodeType.
-}
data NodeType = Mandatory | Optional | Wrapper deriving (Show, Eq)

class Show g => Grammar g where
  nodetypeof :: g -> NodeType
