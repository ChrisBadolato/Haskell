--Christopher Badolato 3064088
--1/31/19  
--Test 1





isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral



isPrime n = ip n [2..(isqrt n)]
 where
 ip _ [] = True
 ip n (x:xs) 
    | n `mod` x == 0 = False 
    | otherwise = ip n xs

test1Problem1 n =  [x | x <- [2..(n)], isPrime x]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

endsIn3 n
 | n `mod` 10 == 3 = True
 | otherwise = False

test1Problem2 n = [x | x <- [fib 0..fib n], endsIn3 x]

multi5 n
 | n `mod` 5 == 0 = True
 | otherwise = False

test1Problem3 n = [x | x <- [1..n], multi5 x]