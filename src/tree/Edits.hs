module Edits where

import Tree
import AST
import Util

data EditType = Identity | Insert | Delete | Move | Update deriving (Eq, Show)
data Edit a = Edit {edittype :: EditType, name :: String, run :: AST a -> AST a} -- inverse :: Edit a
type EditScript a = [Edit a]

-- Turns an edit script into one single function
-- The returned function will run the entire edit script on the given AST.
foldEditScript :: EditScript a -> AST a -> AST a
foldEditScript es = reversefoldr (.) id $ fmap run es

-- The identity of edits
edit_identity :: Edit a
edit_identity = Edit {edittype = Identity, run = id, name = "identity"}

-- Add the tree s as the i-th child of node p
edit_ins_tree :: Show a => AST a -> UUID -> Int -> Edit a
edit_ins_tree s p i = Edit {edittype = Insert,
                            run = manipulate ins,--(fmap $ increaseVersion 1).
                            name = "ins_tree("++(show $ uuidOf s)++", "++(show p)++", "++(show i)++")"} -- inverse = del_tree $ uuidOf s
 where ins x@(Tree n c) = if uuid n == p
                          then Tree n (Util.insertAtIndex i s c)
                          else x

-- delete the subtree rooted in v
edit_del_tree :: UUID -> Edit a
edit_del_tree v = Edit {edittype = Delete,
                        run = (filterTrees ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
                        name = "del_tree("++(show v)++")"}