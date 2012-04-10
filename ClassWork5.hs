module Classwork5 where
import Control.Applicative

instance Applicative (Either e) where
	pure = Right
	(<*>) _ (Left x) _ = Left x
	(<*>) _ _ (Left x) = Left x
	(<*>) f (Right x) (Right y) = Right (f x y)