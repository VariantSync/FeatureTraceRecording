module Edits where

import AST
import FTRNode
import Util

data EditType = Insert | Delete | Move | Update

class Edit e where
  edittype :: e a -> EditType
  edit :: e a -> AST (Node a) -> AST (Node a)

data InsTree a = InsTree {treeToInsert::(AST (Node a)), pos::UUID, index::Int}
instance Edit InsTree where
  edittype _ = Insert
  edit e t = manipulate ins t
    where ins x@(AST n c) = if uuid n == pos e
                            then AST n (Util.insertAtIndex (index e) (treeToInsert e) c)
                            else x

data DelTree a = DelTree UUID
instance Edit DelTree where
  edittype _ = Delete
  edit (DelTree pos) t = AST.filter ((pos /=) . uuidOf) t