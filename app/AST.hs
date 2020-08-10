{-# LANGUAGE DeriveTraversable #-}

module AST where
import Data.List
import Data.Maybe

import Util

data AST a = AST a [AST a] deriving (Eq, Foldable, Traversable)

prettyPrint :: Show a => Int -> AST a -> String
prettyPrint i (AST n []) = (genIndent i) ++ (show n) ++ " []\n"
prettyPrint i (AST n children) =
  (genIndent i) ++ (show n) ++ " [\n" ++ (concat $ fmap (prettyPrint $ i+1) children) ++ (genIndent i) ++ "]\n"

instance Functor AST where
  fmap f (AST n c) = AST (f n) (fmap (fmap f) c)

instance Show a => Show (AST a) where
  show = prettyPrint 0

isleaf :: AST a -> Bool
isleaf (AST _ children) = null children

element :: AST a -> a
element (AST n _) = n

find :: AST a -> (AST a -> Bool) -> Maybe(AST a)
find x@(AST _ children) predicate = case predicate x of
  False -> safehead $ catMaybes $ map (\t -> AST.find t predicate) children
  True -> Just x

parent :: Eq a => AST a -> AST a -> Maybe(AST a)
parent root t = AST.find root (\(AST _ children) -> elem t children)

manipulate :: (AST a -> AST a) -> AST a -> AST a
manipulate f (AST x children) = f (AST x (fmap (manipulate f) children))
