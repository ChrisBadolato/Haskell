--Christopher Badolato 3064088
--COP 4020


--Finds whether a number is prime or not w/ recursion

isqrt :: Integral i => i -> i
isqrt = floor.sqrt.fromIntegral

isPrime n = ip n [2..(isqrt n)]
 where
ip _ [] = True
ip n (x:xs) |n `mod` x == 0 = False | otherwise = ip n xs 


--Finds the factorial of the input value n

factorial n = fact n 1
 where 
fact n result | n > 1 = fact (n-1) (result * n) | otherwise = result



--Finds the fibonacci of the input value n. Will make sure our last value is 0.

fibonacci n = fib n (0,1)
 where
fib n (first, second) | n == 0 = first | otherwise = fib (n-1) (second, first+second)