module Classwork7 where
import Control.Monad

 instance Monad (State s) where
    return a = State (\s -> (a, s))
    (>>=) (State (x)) f = State (\s -> let State g = f (fst x s)) in
        g (snd (x s))

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (mempty, a) 
    (>>=) (Writer (v,a)) f = Writer (mappend v w, b) where
        Writer(w, b) = f a

data Reader r a = Reader (r->a)
instance Monad (Reader r) where
    return a = Reader(\x -> a)
    (>>=) (Reader x) f = Reader $ \r -> let (Reader y) = t (x r) in y r 

space n = mapM (const [1..n]) [1..n]

sieve s = and $ do
    (y1, x1) <- zip s [1..]
    (y2, x2) <- zip s [1..]
    guard (x1 /= x2)
    return $ y1 /= y2 && abs(x1 - x2) /= abs(y1 - y2)

ferz n = filter sieve (space n)