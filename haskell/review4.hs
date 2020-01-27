

isDivisible number divisor = number `mod` divisor == 0

isDivisibleByList number [] = True
isDivisibleByList number (x:xs)
    | isDivisible number x == False = False
    | otherwise = isDivisibleByList number xs



smallestNumber xs = go xs 1
 where go xs n | isDivisibleByList n xs = n | otherwise = go xs (n+1)



sumOfSquares n = sum [x^2 | x <- [1..n]]
squareOfSums n = (sum [x | x <- [1..n]]) ^ 2


difference n = (squareOfSums n) - (sumOfSquares n)


factorList n = [x | x <- [1..n], isDivisible n x]


isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral



isPrime n = ip n [2..(isqrt n)]
 where
 ip _ [] = True
 ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs



primeFactorList n = [x | x <- [2..n], isDivisible n x, isPrime x]



getCountedListOfPrimes n = go n 2
 where 
 go n count 
    | n <= 0 = [] | isPrime count = count : go (n-1) (count+1) | otherwise = go n (count+1)



getLastPrime n = last (getCountedListOfPrimes n)


triangleNumber n = sum [1..n]


howManyFactors n = [length (factorList (triangleNumber x)) | x <- [1..n]]


firstTriangleWithMoreThan500Divisors = go 1
 where 
 go count 
    | triangleNumber count > 500 = triangleNumber count 
    | otherwise = go (count+1)



chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  



numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
 where isLong xs = length xs > 15