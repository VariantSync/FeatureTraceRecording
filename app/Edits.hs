module Edits where

import Tree
import FTRNode
import Util

data EditType = Insert | Delete | Move | Update deriving (Eq, Show)
data Edit a = Edit {edittype :: EditType, name :: String, run :: FTRAST a -> FTRAST a} -- inverse :: Edit a

-- Add the tree s as the i-th child of node p
ins_tree :: Show a => FTRAST a -> UUID -> Int -> Edit a
ins_tree s p i = Edit {edittype = Insert, run = (fmap $ increaseVersion 1).(manipulate ins), name = "ins_tree("++(show $ uuidOf s)++", "++(show p)++", "++(show i)++")"} -- inverse = del_tree $ uuidOf s
 where ins x@(Tree n c) = if uuid n == p
                         then Tree n (Util.insertAtIndex i s c)
                         else x

-- delete the subtree rooted in v
del_tree :: UUID -> Edit a
del_tree v = Edit {edittype = Delete, run = (fmap $ increaseVersion 1).(filterTrees ((v /=) . uuidOf)), name = "del_tree("++(show v)++")"}