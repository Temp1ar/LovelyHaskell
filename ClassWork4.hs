module Classwork4 where
import Data.List
import Data.Monoid

repeater n = foldr ((++) . replicate n) []

length' = foldr ((+) . const 1) 0

or' = foldr (||) False

head' = foldr1 (const)

last' = foldr1 (flip const)

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (max)

map' f = foldr ((++) . return . f) []

filter' f = foldr((++) . (\x -> if (f x) then (return x) else [])) []

reverse' = foldr (\x xs -> xs ++ (return x)) []
reverse'2 = foldr (flip (++) . return) []

reverse'' = foldl (\xs x -> (return x) ++ xs) []

join list = (\l -> if null l then l else init l) $ foldr ((++) . flip (++) ",") [] list

foldl'' f z list  = foldr (flip f) z $ reverse list
foldl''2 f z list = foldr (\x xs -> f (take (length xs) (return x ++ xs)) (last xs)) z list
-- 1 `f` (2 `f` (3 `f` nil)) foldr
-- f 1 (f 2 (f 3 nil))
-- (((nil `f` 1) `f` 2) `f` 3) foldl
-- f (f (f nil 1) 2) 3
-- ^^^^^^^^^^^^^^^^^^^
-- f (f (f nil 3) 2) 1

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

tree = Branch (Branch (Branch Nil 1 Nil) 3 (Branch Nil 1 Nil)) 5 (Branch Nil 7 ((Branch Nil 1 Nil)))

foldTree :: (b->a->b->b) -> b -> Tree a -> b
foldTree f nil Nil = nil
foldTree f nil (Branch left a right) = f (foldTree f nil left) a (foldTree f nil right)
-- usage: foldTree (\left a right-> left + a + right) 0 tree

--preorder
flattenTreePre  = foldTree (\left a right -> (return a) ++ left ++ right) []

--inorder
flattenTreeIn   = foldTree (\left a right -> left ++ (return a) ++ right) []

--postorder
flattenTreePost = foldTree (\left a right -> left ++ right ++ (return a)) []

--levelorder?


-- В левом и правом поддереве кол-во узлов отличается 
-- не более чем на 1 для любого узла