module KorovinHomework3 where
import Data.List

repeater :: [a] -> Int -> [a]
repeater list n = concat $ map (replicate n) list 

removeEveryNth :: [a] -> Int -> [a]
removeEveryNth list n = snd $ unzip $ filter ((/= n-1) . fst) $ zip (cycle [0..n-1]) list

substr :: [a] -> Int -> Int -> [a]
substr list s e = (take (e-s) . drop s) list

shiftLeft :: [a] -> Int -> [a]
shiftLeft list n | n >= 0 = substr (cycle list) nModLength $ length list + nModLength
                 | n < 0  = shiftLeft list $ length list + nModLength
    where nModLength = n `mod` length list

removeAndReturn :: Int -> [a] -> (a, [a])
removeAndReturn n list = (list !! n, fst split ++ drop 1 (snd split))
    where split = splitAt n list

combinations :: Int -> [a] -> [[a]]
combinations n list = filter ((==n) . length) $ subsequences list

data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Eq)

instance Show a => Show (Tree a) where
    showsPrec _ = myShowsTree

myShowsTree Empty = ("." ++)
myShowsTree (Branch left x right) = ('<' :) 
                            . myShowsTree left
                            . ('{' :) . (shows x) . ('}' :)
                            . myShowsTree right
                            . ('>' :)   

-- List of all completely balanced trees
-- List of all trees?
--expandRight Empty = Branch Empty () Empty
--expandRight (Branch left () right) = (Branch left () (expandRight right))

--expandLeft Empty = Branch Empty () Empty
--expandLeft (Branch left () right) = (Branch (expandLeft left) () right)
-- Maybe we should use iterate with those functions ?

--expand Empty = ([Branch Empty () Empty] ++)
--expand (Branch left () right) = (expandTree left) ++ (expandTree right)

-- Is tree structural symmetric
testSym = Branch (Branch (Branch Empty () Empty) () Empty) () (Branch Empty () (Branch Empty () Empty)) 

isSymmetric Empty = True
isSymmetric (Branch left _ right) = structureSym left right

structureSym Empty Empty = True
structureSym (Branch left1 _ right1) (Branch left2 _ right2) =
    structureSym left1 right2 && structureSym right1 left2
structureSym _ _ = False

-- Binary search tree
ins :: Ord a => a -> Tree a -> Tree a
ins element Empty = Branch Empty element Empty
ins element (Branch left root right) 
    | element == root = Branch left root right
    | element < root  = Branch (ins element left) root right
    | element > root  = Branch left root (ins element right) 

tree = Empty

listToBST :: Ord a => [a] -> Tree a
listToBST list = foldl (flip ins) Empty list

-- List of all balanced trees