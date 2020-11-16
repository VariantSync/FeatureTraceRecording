module Defunctor where

class Defunctor f where
    demap :: (f a -> f b) -> a -> b

instance Defunctor Maybe where
    demap f x = case f (Just x) of
        Just y -> y
        Nothing -> error "Value cannot be defunctorialized!"

instance Defunctor [] where
    demap f x = head $ f [x]
