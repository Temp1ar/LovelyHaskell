module Classwork6 where
import Control.Monad

j17 = do
    x <- Just 17
    return (x < 21)

l123 = do
    x <- [1, 2, 3]
    [x, 2*x]

l12 = do
    n <- [1, 2]
    c <- ['a', 'b']
    return (n, c)

filter' :: (a->Bool) -> [a] -> [a]
filter' p xs = do 
	x <- xs
	True <- return (p x)
	return x

(>=:>) f g v = f v >>= g

join' x = x >>= id

join'' x = do
    x <- id x
    x

---- | This generalizes the list-based 'filter' function.

--filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
--filterM _ []     =  return []
--filterM p (x:xs) =  do
--   flg <- p x
--   ys  <- filterM p xs
--   return (if flg then x:ys else ys)

-- Предикат (\x -> [True, False]) примененный к любому значению
-- вернет список [True, False]. Т.е. flg <- [True, False] будет попеременно
-- True или False. Вторая строчка рекурсивно запускает функцию на хвосте списка 
-- и возвращает справа от стрелки эту же функцию примененную на хвосте.
-- return в зависимости от flg либо включает в список текущее число, либо игнорирует
-- его.

-- Законы
-- (>=:>) f g v = f v >>= g
--
-- return >=> k $ a ~ return a >>= k ~ k a
-- k >=> return $ a ~ k a >>= return ~ k a
-- (u >=> v $ a) >=> w $ b ~ (u a) >>= v b >>= w ~ (u a) >=> (v >=> w $ b)

-- (>=>) через join и fmap
-- (>=|>) f g v = ? 

fmapM f xs = do
    x <- xs
    return $ f x

-- Homework
f n list = list >>= replicate n

mults n = do 
    x <- [1..n]
    y <- [1..n]
    guard((x * y == n) && (x < y))
    return (x, y)

abss list = do
    x <- zip list $ tail list
    return $ abs $ (fst x) - (snd x)

-- Нужно показать, что в классе Monad можно реализовать pure и <*>
-- pure очевидно есть return в монаде
-- попробуем написать (<*>) :: f (a -> b) -> f a -> f b
--monadFunc <*> monadX = do
--    func <- monadFunc
--    x <- monadX
--    return $ func x
--
-- *Classwork6> [(*3), (*2)] <*> [1, 2, 3]
-- [3,6,9,2,4,6]

