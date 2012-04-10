module Factorial(factorial) where
import Data.Function(fix)

factorial' = \f n -> if (n == 0)
               then 1
			   else (n * f(n - 1))

factorial n = fix factorial' n