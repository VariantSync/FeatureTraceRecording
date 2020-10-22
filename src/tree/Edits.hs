module Edits where

import UUID
import Tree
import AST
import Util
import ListUtil

import Data.List
import Data.Set

data EditType = Identity | TraceOnly | Insert | Delete | Move | Update deriving (Eq, Show)
data Edit g a = Edit {
    edittype :: EditType,
    name :: String,
    run :: AST g a -> AST g a,
    delta :: AST g a -> Set (Node g a)} -- inverse :: Edit a
type EditScript g a = [Edit g a]

instance Show (Edit g a) where
    show = name

-- Turns an edit script into one single function
-- The returned function will run the entire edit script on the given AST.
foldEditScript :: EditScript g a -> AST g a -> AST g a
foldEditScript es = reversefoldr (.) id $ run <$> es

-- The identity of edits
edit_identity :: Edit g a
edit_identity = Edit {
    edittype = Identity,
    run = id,
    delta = \_ -> empty,
    name = "identity"}

{-
An identity edit that will keep the given set of nodes as delta for the feature trace recording.
Upon recording, all given nodes will have their feature trace changed to the feature context.
This function assumes that the given set of nodes is a subset of the nodes in the future edited tree.
-}
edit_trace_only :: Set (Node g a) -> Edit g a
edit_trace_only nodes = Edit {
    edittype = TraceOnly,
    run = id,
    delta = \_ -> nodes,
    name = "tracechange"}

-- Add the tree s as the i-th child of node p
edit_ins_tree :: (Eq a) => AST g a -> UUID -> Int -> Edit g a
edit_ins_tree stree p i = Edit {
    edittype = Insert,
    run = \t -> if any ((p==).uuid) t then manipulate ins t else error $ "The given parent node "++(show p)++" does not exist!",
    delta = \t -> if any ((p==).uuid) t then toset stree else empty,
    name = "ins_tree("++(intercalate ", " $ show <$> [uuidOf stree, p, i])++")"} -- inverse = del_tree $ uuidOf s
    where ins x@(Tree n c) = if uuid n == p
                             then Tree n (ListUtil.insertAtIndex i stree c)
                             else x

{-
Replaces the children of node p \in T in range [i, j] (i <= j)
with the new tree stree nosubtreeof T, then located at index i.
The replaced children are added as children of snode \in stree at index k preserving their order.
If p = epsilon, root stree becomes the new root of T.
-}
-- TODO: Do we want to split ins_partial into two edits: one where p = epsilon and one where p != epsilon?
-- ins_partial with p = epsilon will never occur if we have immutable root nodes such as "file" or an empty abstract project root node as in Ecco.
edit_ins_partial :: (Eq a) => AST g a -> UUID -> Int -> Int -> UUID -> Int -> Edit g a 
edit_ins_partial stree p i j snode k = Edit {
    edittype = Insert,
    run = if p == epsilon
          then \t -> manipulate (insertMovedChildren [t]) stree
          else manipulate insp,
    delta = \t -> if p == epsilon || any ((p==).uuid) t
                  then toset stree
                  else empty,
    name = "ins_partial("++(intercalate ", " $ show <$> [uuidOf stree, p, i, j, snode, k])++")"}
    where insp t@(Tree n c) = if uuid n == p
                              then Tree n (insertAtIndex i (newSubTreeWith $ getRange i j c) $ removeRange i j c)
                              else t
          newSubTreeWith children = manipulate (insertMovedChildren children) stree
          insertMovedChildren children t@(Tree n c) = 
                                       if uuid n == snode
                                       then Tree n (insertListAtIndex k children c)
                                       else t

-- delete the node v and move its children up
edit_del_node :: (Eq a) => UUID -> Edit g a
edit_del_node v = Edit {
    edittype = Delete,
    run = (filterNodes ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
    delta = \t -> case findById v t of
        Nothing -> empty
        Just t' -> singleton $ element t',
    name = "del_node("++(show v)++")"}

-- delete the subtree rooted in v
edit_del_tree :: (Eq a) => UUID -> Edit g a
edit_del_tree v = Edit {
    edittype = Delete,
    run = (filterTrees ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
    delta = \t -> case findById v t of
        Nothing -> empty
        Just t' -> toset t',
    name = "del_tree("++(show v)++")"}

-- This commented out signature is the signature of move_tree in the paper.
-- Actually, the given subtree should just be an index because the subtree is present in the given tree.
-- edit_move_tree :: (Eq a) => AST a -> UUID -> Int -> Edit a
edit_move_tree :: (Grammar g, Eq a, Show a) => UUID -> UUID -> Int -> Edit g a
edit_move_tree s p i = Edit {
    edittype = Move,
    run = \t -> case streeIn t of
        Just stree -> (ins stree).del $ t
        Nothing -> error $ "The subtree "++(show s)++" cannot be moved because it does not exist in the given tree \n"++(show t),
    delta = \t -> case streeIn t of
        Just stree -> toset stree
        Nothing -> empty,
    name = "move_tree("++(intercalate ", " $ show <$> [s, p, i])++")"}
    where streeIn = findById s
          del = run $ edit_del_tree s
          ins t = run $ edit_ins_tree t p i

edit_update :: (Grammar g, Show a, Eq a) => UUID -> g -> a -> Edit g a
edit_update id newGrammarType newVal = Edit {
    edittype = Update,
    run = \t -> (\n ->
        if uuid n == id
        then Node {value = newVal, grammartype = newGrammarType, uuid = uuid n}
        else n) <$> t,
    delta = \t -> case findById id t of
        Nothing -> empty
        Just x -> toset x,
    name = "update("++(intercalate ", " $ [show id, show newGrammarType, show newVal])++")"
}
