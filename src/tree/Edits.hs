-- | Module for edits to ASTs.
module Edits where

import UUID
import Tree
import AST
import Grammar

import Util
import ListUtil

import Data.List
import Data.Set

-- | Each edit is associated to a type depending on what it does.
data EditType
    = Identity  -- ^ @Identity@ edits do nothing to the AST (they are noops).
    | TraceOnly -- ^ @TraceOnly@ are edits that do not alter the AST but have a non-empty delta. This delta is used to descibe side-effects that should be applied to all nodes in the delta (e.g., change their feature mapping).
    | Insert    -- ^ @Insert@ edits only add nodes to a tree.
    | Delete    -- ^ @Delete@ edits only remove nodes from a tree.
    | Move      -- ^ @Move@s only relocate nodes within the same tree.
    | Update    -- ^ @Update@s only change the contents of one or more nodes without altering the tree structure.
    deriving (Eq, Show)

-- | An edit to an AST.
data Edit g a = Edit {
    -- | The type of this edit classifying its behaviour.
    edittype :: EditType,
    -- | The name of this edit. Used for debugging and printing.
    name :: String,
    -- | Applies the edit to an AST, yielding the edited AST.
    run :: AST g a -> AST g a,
    -- | Computs the set of all nodes that will be altered (inserted, deleted, moved, updated ...) when applying the edit to a given AST.
    delta :: AST g a -> Set (Node g a)
}

-- | An @EditScript@ is a sequence of edits that should be applied in order to a single AST.
type EditScript g a = [Edit g a]

instance Show (Edit g a) where
    show = name

-- | Runs an entire edit script on a given AST.
-- If curried, turns an edit script into one single function that can run that script on any AST.
foldEditScript :: EditScript g a -> AST g a -> AST g a
foldEditScript es = reversefoldr (.) id $ run <$> es

-- | The identity of edits.
-- Does nothing and has an empty delta for all trees.
edit_identity :: Edit g a
edit_identity = Edit {
    edittype = Identity,
    run = id,
    delta = \_ -> empty,
    name = "identity"}

{-
| An identity edit that will keep the given set of nodes as delta for the feature trace recording.
Upon recording, all given nodes will have their feature trace changed to the feature context.
This function assumes that the given set of nodes is a subset of the nodes in the future edited tree.
-}
edit_trace_only :: Set (Node g a) -> Edit g a
edit_trace_only nodes = Edit {
    edittype = TraceOnly,
    run = id,
    delta = \_ -> nodes,
    name = "tracechange"}

-- | Inserts a subtree into another tree.
-- The given tree (first argument) will become the @i@-th child of the node with the given UUID.
edit_ins_tree :: (Eq a) => AST g a -> UUID -> Int -> Edit g a
edit_ins_tree stree p i = Edit {
    edittype = Insert,
    run = \t -> if any ((p==).uuid) t then manipulate ins t else error $ "The given parent node "++(show p)++" does not exist!",
    delta = \t -> if any ((p==).uuid) t then toset stree else empty,
    name = "ins_tree("++(intercalate ", " $ show <$> [uuidOf stree, p, i])++")"} -- inverse = del_tree $ uuidOf s
    where ins x@(Tree n c) = if uuid n == p
                             then Tree n (ListUtil.insertAtIndex i stree c)
                             else x

-- | Delete the node with the given UUID v and move its children up (such that they become children of the deleted node's parent).
edit_del_node :: (Eq a) => UUID -> Edit g a
edit_del_node v = Edit {
    edittype = Delete,
    run = (filterNodes ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
    delta = \t -> case findById v t of
        Nothing -> empty
        Just t' -> singleton $ element t',
    name = "del_node("++(show v)++")"}

-- | Delete the entire subtree whose root has the given id.
edit_del_tree :: (Eq a) => UUID -> Edit g a
edit_del_tree v = Edit {
    edittype = Delete,
    run = (filterTrees ((v /=) . uuidOf)), --(fmap $ increaseVersion 1).
    delta = \t -> case findById v t of
        Nothing -> empty
        Just t' -> toset t',
    name = "del_tree("++(show v)++")"}

-- | Moves the subtree @s@ with the given root (first argument).
-- @s@ will become the i-th child of the node with the given id (second argument).
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

-- | Updates the node with the given UUID to have a new grammar type and a new value.
-- The node will keep its UUID (i.e., not get a new uuid as it is still associated to the same previous node.
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
