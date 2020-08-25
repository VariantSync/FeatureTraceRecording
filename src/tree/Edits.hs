module Edits where

import UUID
import Tree
import AST
import Util

import Data.Set

data EditType = Identity | TraceOnly | Insert | Delete | Move | Update deriving (Eq, Show)
data Edit a = Edit {edittype :: EditType, name :: String, run :: AST a -> AST a, delta :: AST a -> Set (Node a)} -- inverse :: Edit a
type EditScript a = [Edit a]

-- Turns an edit script into one single function
-- The returned function will run the entire edit script on the given AST.
foldEditScript :: EditScript a -> AST a -> AST a
foldEditScript es = reversefoldr (.) id $ run <$> es

-- The identity of edits
edit_identity :: Edit a
edit_identity = Edit {edittype = Identity,
                      run = id,
                      delta = \t -> empty,
                      name = "identity"}

{-
An identity edit that will keep the given set of nodes as delta for the feature trace recording.
Upon recording, all given nodes will have their feature trace changed to the feature context.
This function assumes that the given set of nodes is a subset of the nodes in the future edited tree.
-}
edit_trace_only :: Set (Node a) -> Edit a
edit_trace_only nodes = Edit {edittype = TraceOnly,
                              run = id,
                              delta = \t -> nodes,
                              name = "tracechange"}

-- Add the tree s as the i-th child of node p
edit_ins_tree :: (Eq a, Show a) => AST a -> UUID -> Int -> Edit a
edit_ins_tree s p i = Edit {edittype = Insert,
                            run = manipulate ins,--(fmap $ increaseVersion 1).
                            delta = \t -> toset s,
                            name = "ins_tree("++(show $ uuidOf s)++", "++(show p)++", "++(show i)++")"} -- inverse = del_tree $ uuidOf s
                            where ins x@(Tree n c) = if uuid n == p
                                                     then Tree n (Util.insertAtIndex i s c)
                                                     else x

-- delete the subtree rooted in v
edit_del_tree :: (Eq a) => UUID -> Edit a
edit_del_tree v = Edit {edittype = Delete,
                        run = (filterTrees ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
                        delta = \t -> case find t (\z -> v == uuidOf z) of
                            Nothing -> empty
                            Just t' -> toset t',
                        name = "del_tree("++(show v)++")"}