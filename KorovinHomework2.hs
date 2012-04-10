module KorovinHomework2 where
import Data.Char 
import Data.List

data Direction = North | East | South | West deriving Show
data Locator = L Direction deriving Show

turnCW :: Locator -> Locator
turnCW (L North) = (L East)
turnCW (L East)  = (L South)
turnCW (L South) = (L West)
turnCW (L West)  = (L North)

turnCCW :: Locator -> Locator
turnCCW (L North) = (L West)
turnCCW (L West)  = (L South)
turnCCW (L South) = (L East)
turnCCW (L East)  = (L North)

turn180 :: Locator -> Locator
turn180 (L North) = (L South)
turn180 (L West)  = (L East)
turn180 (L South) = (L North)
turn180 (L East)  = (L West)

--
splitToDigits :: Integral a => a -> [Int]
splitToDigits n = map digitToInt $ show $ abs n

--
hasAllDigits :: Integral a => a -> Bool
hasAllDigits n = null $ [1..9] \\ splitToDigits n

--
hasAllDigitsOnce :: Integral a => a -> Bool
hasAllDigitsOnce n = (null $ [1..9] \\ unique_digits) && (null $ digits \\ unique_digits)
    where digits = filter (/=0) $ splitToDigits n
          unique_digits = nub digits

--
listOfLists :: [a] -> Int -> [[a]]
listOfLists list n = snd $ mapAccumL (\x y -> (x+1,drop (x-n) $ take x list)) n (take (length list - n + 1) list)

--
data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving Show

--        1
--       /  \
--      2    3
--    /  \     \
--   4    5     8
--             /  \
--            6    7
tree = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 8 (Node 6 Nil Nil) (Node 7 Nil Nil)) Nil)

sumOfTree :: Num a => BinaryTree a -> a
sumOfTree Nil = 0
sumOfTree (Node a left right) = a + sumOfTree left + sumOfTree right

heightOfTree :: BinaryTree a -> Int
heightOfTree Nil = 0
heightOfTree (Node _ left right) = 1 + max (heightOfTree left) (heightOfTree right)

widthOfLevel :: BinaryTree a -> Int -> Int
widthOfLevel Nil _ = 0
widthOfLevel _ 1 = 1
widthOfLevel (Node _ left right) n = widthOfLevel left (n-1) + widthOfLevel right (n-1)

widthOfTree :: BinaryTree a -> Int
widthOfTree Nil = 0
widthOfTree cur@(Node _ left right) = maximum [widthOfLevel cur n | n <- [1..heightOfTree cur]]