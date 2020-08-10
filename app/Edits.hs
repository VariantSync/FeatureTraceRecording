module Edits where

import AST
import FTRNode
import Util

class Edit e where
  edit :: e a -> AST (Node a) -> AST (Node a)

data InsTree a = InsTree {tree::(AST (Node a)), pos::UUID, index::Int}
instance Edit InsTree where
  edit e t = manipulate
    (\x@(AST n c) -> if uuid n == pos e then
      (AST n (Util.insertAtIndex (index e) (tree e) c))
    else
      x)
    t

