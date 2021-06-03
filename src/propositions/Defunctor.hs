{- |
Description: Type class for de-lifting functions from embellished types.
License: GNU LGPLv3
Maintainer: paul.bittner@uni-ulm.de

Module for type class 'Defunctor' and some instances for Prelude types.
'Defunctor's can de-lift functions from embellished types to plain types.
-}
module Defunctor where

{- |
A 'Defunctor' is a type that allows de-lifting functions from embellished types to functions on the types that are embellished.
This can be seen sort of an inverse of a functor that is not always possible to construct.
-}
class Defunctor f where
    -- | Inverse of fmap. Takes a function on embellished values (e.g., a functor or monad) and turns it into a plain function on values.
    -- This inversion is usually impossible or only a partial function.
    demap :: (f a -> f b) -> a -> b

-- | 'Maybe' is a 'Defunctor'.
instance Defunctor Maybe where
    -- | Applies the given function to the value. Throws an error if the function returned 'Nothing'.
    demap f x = case f (Just x) of
        Just y -> y
        Nothing -> error "Value cannot be defunctorialized!"

-- | Lists are 'Defunctor's.
instance Defunctor [] where
    -- | Wraps the argument in a list, applies the function and unwraps it.
    demap f x = head $ f [x]
