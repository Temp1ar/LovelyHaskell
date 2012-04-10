module Fibonacci where
fibonacci n = 
	if n > 1
	then fibonacci (n - 1) + fibonacci(n - 2)
	else 1