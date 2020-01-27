
doublePos' [] = []
doublePos' (x:xs) = x * 2 : doublePos' xs

spaces' 0 = []
spaces' n = ' ' : spaces' (n-1)

fibList n = [1,1] ++ doFib 1 2 n
 where doFib f s n | (f+s) > n = [] | otherwise = (f+s) : doFib s (f+s) n

fibs n = sum [ x | x <- (fibList n), even x]


isPrime n = ip n 2
 where ip n f | f > (n `div` 2) = True | n `mod` f == 0 = False | otherwise = ip n (f+1)

isPalindrome n = show(n) == reverse(show n)

listOfPrimes n = [ x | x <- [2..n], isPrime x, isPalindrome x]




