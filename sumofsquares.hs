module SumOfSquare(sumOfSquare) where
import Data.Function(on)

sumOfSquare = (+) `on` (^ 2)