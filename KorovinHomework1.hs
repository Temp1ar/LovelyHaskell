module KorovinHomework1 where

-- First problem
density x = if x >= 3 && x <=4 then 1 else 0

-- Second problem
gcd' a b = if b /= 0 then gcd' b $ a `mod` b else a

-- Third problem
summOfDigits a = 
    if a /= 0 
    then (abs a) `mod` 10 + summOfDigits ( (abs a) `div` 10 )
    else 0

numOfDigits a = 
    if a == 0 then 1 else numInner (abs a)
    where numInner a =        
            if a /= 0 then 1 + numInner (a `div` 10) else 0
        
-- Fourth problem
-- slow solution
recurs n 
    | n == 1 = 1
    | n == 2 = 2
    | n == 3 = 3
    | otherwise = recurs (n - 1) + recurs (n - 2) - 2 * recurs (n - 3)
    
-- fast solution
recurs' n
    | n < 4 = n
    | otherwise = recursWorker n 3 2 1 
    where
        recursWorker n nm1 nm2 nm3
            | n > 3 = recursWorker (n-1) (nm1 + nm2 - 2*nm3) nm1 nm2
            | otherwise = nm1

-- Fifth problem
trapez f a b step 
    | step <= 0 = undefined
    | b < a = undefined
    | otherwise = trapez_helper f a b step 0
        where
        trapez_helper f a b step sum 
            | a >= b = sum
            | otherwise = trapez_helper f (a+step) b step (sum + step * (f a + f (a+step)) / 2)

-- Sixth problem
newton f f' x0 eps =
    converge (newtonCore f f' x0) eps
    where
        converge (prev:next:xn) eps =
            if (abs(next-prev) / prev) <= eps
            then next
            else converge (next:xn) eps
        newtonCore f f' x0 = x1:xn
            where x1 = x0 - f x0 / f' x0
                  xn = newtonCore f f' x1
                  
newton_square n 
    | n == 0 = 0
    | n < 0 = undefined
    | otherwise = newton (\x -> x*x - abs(n)) (\x -> 2*x) (abs (n/2)) 1e-3
