module Classwork5 where

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
infixl 4 <*>
g <$> x = pure g <*> x

instance Applicative (Either e) where
	pure = Right
	(<*>) (Left x) _ = Left x
	(<*>) (Right f) x = fmap f x

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving Show
instance Functor Tree where
    fmap g Nil = Nil
    fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

-- Behaviour like in zip list
instance Applicative Tree where
    pure x = Branch (pure x) x (pure x)
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch lf f rf) (Branch lv v rv) = Branch (lf <*> lv) (f v) (rf <*> rv)

--(+) <$> Branch Nil 5 (Branch Nil 2 Nil) <*> Branch Nil 3 (Branch Nil 2 Nil)
--Branch Nil 8 (Branch Nil 4 Nil)

newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)


x >*< y = getZipList (ZipList x <*> ZipList y)
infixl 4 >*<

(>$<) :: (a -> b) -> [a] -> [b]
g >$< x = (getZipList $ pure g) >*< x


x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]


-- Composition
--Prelude Control.Applicative> (.) <$> Just (+2) <*> Just (*5) <*> Just 8
--Just 42

--Prelude Control.Applicative> (.) <$> [(+2)] <*> [(*5)] <*> [8]
--[42]

-- Interchange
-- ???