module Util where

genIndent :: Int -> String
genIndent i = concat $ replicate i " "

safeFromJust :: Maybe a -> (() -> a) -> a
safeFromJust Nothing gen = gen ()
safeFromJust (Just x) _ = x

reversefoldr :: (a -> b -> b) -> b -> [a] -> b
reversefoldr f zero container = foldr f zero $ reverse container

parenIf :: Bool -> String -> String
parenIf True s = "("++s++")"
parenIf False s = s

takeIf :: (a -> Bool) -> Maybe a -> Maybe a
takeIf _ Nothing = Nothing
takeIf p (Just x)
    | p x = Just x
    | otherwise = Nothing

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p a
    | p a = Nothing
    | otherwise = Just a

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast [] = []
removeFirstAndLast [x] = []
removeFirstAndLast xs = tail $ init xs

removeQuotes :: String -> String
removeQuotes s =
    if head s == '\"' && last s == '\"'
    then removeFirstAndLast s
    else s

-- Here are some exercises
{-

data List a = Empty | Head a | Tail a (List a)

instance Semigroup (List a) where
    Empty <> l = l
    l <> Empty = l
    (Head x) <> l = Tail x l
    (Tail x xs) <> l = Tail x (xs <> l)

instance Monoid (List a) where
    mempty = Empty
    mappend = (<>)

instance Functor List where
    fmap f Empty = Empty
    fmap f (Head x) = Head (f x)
    fmap f (Tail x l) = Tail (f x) (fmap f l)

instance Applicative List where
    pure x = Head x
    Empty <*> l = Empty
    (Head f) <*> l = fmap f l
    (Tail f fs) <*> l = (fmap f l) <> (fs <*> l)

instance Monad List where
    return = pure
    -- (>>=)       :: forall a b. m a -> (a -> m b) -> m b
    Empty >>= f = Empty
    (Head x) >>= f = f x
    (Tail x xs) >>= f = f x <> (xs >>= f)

data Nullable a = Null | Actually a

instance Semigroup a => Semigroup (Nullable a) where
    Null <> n = n
    n <> Null = n
    (Actually a) <> (Actually b) = Actually (a <> b)

instance Semigroup a => Monoid (Nullable a) where
    mempty = Null
    mappend = (<>)

instance Functor Nullable where
    fmap f Null = Null
    fmap f (Actually x) = Actually (f x)

instance Applicative Nullable where
    pure x = Actually x
    Null <*> n = Null
    (Actually f) <*> n = fmap f n

instance Monad Nullable where
    return = pure
    Null >>= f = Null
    Actually x >>= f = f x
--}