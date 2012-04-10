module ClassWork2 where

-- My own functions
sumOfEven :: Integral a => [a] -> a
sumOfEven [] = 0
sumOfEven (x:xs)
    | even x = x + (sumOfEven xs)
    | otherwise = sumOfEven xs


oddList :: Integral a => [a] -> [a]
oddList [] = []
oddList (x:xs) 
    | odd x = x : oddList xs
    | otherwise = oddList xs

    
swapPairs :: [a] -> [a]
swapPairs [] = []
swapPairs (x:y:xs) =  y : x : swapPairs xs
swapPairs (x:xs) =  x : xs

sumTwoLists :: Num a => [a] -> [a] -> [a]
sumTwoLists [] [] = []
sumTwoLists s@(x:xs) [] = s
sumTwoLists [] s@(x:xs) = s
sumTwoLists (x:xs) (y:ys) = (x + y) : (sumTwoLists xs ys)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

myTake :: [a] -> Int -> [a]
myTake  _ 0 = []
myTake [] _ = []
myTake (x:xs) n 
    | n < 0 = []
    | otherwise = x : myTake xs (n-1)

-- With standart functions
doubleList :: Num a => [a] -> [a]
doubleList = map (*2)
doubleList' :: Num a => [a] -> [a]
doubleList' l = [x*2 | x <- l]

-- Helper function
mapIf p f = map (\x -> if p x then f x else x)

doubleEven :: Integral a => [a] -> [a]
doubleEven = mapIf even (*2)

nullOdd :: Integral a => [a] -> [a]
nullOdd = mapIf odd (*0)

numerize :: [a] -> [(Int,a)]
numerize = zip [0..]

nullOddIndexed l = [ snd x | x <- map func ( numerize l ) ]
    where func (x,y) = if odd x then (x, 0) else (x, y)

removeGreaterThan k = filter (<=k)

applyCondition f x = map (f x)
-- usage applyCondition (>) 3 [1,2,3,4,5,6] 
-- [True,True,False,False,False,False]

