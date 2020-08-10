{-# LANGUAGE DeriveTraversable #-}

module AST where
import Data.List
import Data.Maybe

import Util

data AST a = AST a [AST a] deriving (Eq, Traversable)

prettyPrint :: Show a => Int -> AST a -> String
prettyPrint i (AST n []) = (genIndent i) ++ (show n) ++ " []\n"
prettyPrint i (AST n children) =
  (genIndent i) ++ (show n) ++ " [\n" ++ (concat $ fmap (prettyPrint $ i+1) children) ++ (genIndent i) ++ "]\n"

instance Show a => Show (AST a) where
  show = prettyPrint 0

instance Functor AST where
  fmap f (AST n c) = AST (f n) (fmap (fmap f) c)

instance Foldable AST where
  foldMap f {- a to Monoid -} (AST x c) = mappend (f x) (mconcat $ fmap (foldMap f) c)

instance Applicative AST where
   pure a = AST a []
   (<*>) (AST f cf) (AST x cx) = AST (f x) (concatMap (\g -> fmap (\c -> g <*> c) cx) cf)

--instance Traversable AST where
--  traverse g (AST x []) = fmap pure (g x)
--  traverse g t@(AST x c) = fmap (traverse g) c
  
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

filter :: (AST a -> Bool) -> AST a -> AST a
filter p t = manipulate (\(AST n c) -> AST n (Data.List.filter p c)) t