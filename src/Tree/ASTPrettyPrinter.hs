{- |
Description: Type class for pretty printing 'AST's.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Type class for pretty printing 'AST's.
-}
module Tree.ASTPrettyPrinter where

import Tree.AST
import Tree.Grammar
import Util

-- | An ASTPrettyPrinter can pretty print ASTs of a certain grammar.
class Show g => ASTPrettyPrinter g where
  {- |
  Pretty prints the given AST (last argument) to another type b (e.g., @String@).
  Parameters are:
  * the start indent (e.g., @""@),
  * an indent generator that can generate an indent of a certain width within the context of printing a certain node (e.g., @(\_ i -> genIndent i)@),
  * a function to lift strings to the output type b within the context of printing a certain node,
  * a function print nodes, and
  * the AST to print.
  -}
  prettyPrint :: (Monoid b) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b

-- | Default implementation for pretty printing an AST to a @String@.
showCode :: (Show a, Grammar g, ASTPrettyPrinter g) => AST g a -> String
showCode = showCodeAs "" (\_ i -> genIndent i) (\_ s -> s) show

-- | Alias for 'prettyPrint'.
showCodeAs :: (Monoid b, ASTPrettyPrinter g) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b
showCodeAs = prettyPrint