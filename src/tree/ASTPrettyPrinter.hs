module ASTPrettyPrinter where

import AST
import Grammar
import Util

class Show g => ASTPrettyPrinter g where
  prettyPrint :: (Monoid b) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b

showCode :: (Show a, Grammar g, ASTPrettyPrinter g) => AST g a -> String
showCode = showCodeAs "" (\_ i -> genIndent i) (\_ s -> s) show

showCodeAs :: (Monoid b, ASTPrettyPrinter g) => b -> (Node g a -> Int -> b) -> (Node g a -> String -> b) -> (Node g a -> b) -> AST g a -> b
showCodeAs = prettyPrint