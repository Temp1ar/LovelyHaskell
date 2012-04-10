module ClassWork3 where
data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

elemTreeDFS (Leaf a) x = (a == x)
elemTreeDFS (Branch l e r) x = (e == x) || elemTreeDFS l x || elemTreeDFS r x

-- O(n^2)
findOnLevel (Leaf node) x _ = (node == x)
findOnLevel (Branch _ node _) x 1 = (node == x)
findOnLevel (Branch left node right) x n = findOnLevel left x (n-1) || findOnLevel right x (n-1)

elemTreeBFS tree x = bfsHelper tree x 1
    where bfsHelper (Leaf a) x _ = (a == x)
          bfsHelper s@(Branch _ _ _) x n = findOnLevel s x n || bfsHelper s x (n+1)

tst n  = Branch (tst n)  n (tst (n+1))
finite = Branch (Leaf 2) 1 (Leaf 3)

myfmap f (Leaf node) = Leaf (f node)
myfmap f (Branch left node right) = Branch (myfmap f left) (f node) (myfmap f right) 

instance Functor Tree where
    fmap = myfmap

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
    showsPrec _ = myShowsList
    
myShowsList Nil = ("|" ++)
myShowsList (Cons x xs) = ('<' :) 
                            . shows x
                            . myShowsList xs
                            . ('>' :)                


lst = Cons 2 (Cons 3 (Cons 5 Nil)) 
    
stree = Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5))
instance Show a => Show (Tree a) where
    showsPrec _ = myShowsTree

myShowsTree (Leaf x) = (show x ++)
myShowsTree (Branch left x right) = ('<' :) 
                            . myShowsTree left
                            . ('{' :) . (shows x) . ('}' :)
                            . myShowsTree right
                            . ('>' :)   

                           