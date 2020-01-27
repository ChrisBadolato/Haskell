reversedNumber n = (read (reverse (show n))) + 0

checkDigits [] = True
checkDigits (n:ns) = if even ((fromEnum n) - 48) then False else checkDigits ns

isReversible n = checkDigits (show ((n + (reversedNumber n))))

countReversibles n = [x | x <- [1..(n-1)], isReversible x, x < reversedNumber x]


--Is Palindrome
isPalindrome n = (show n) == (reverse(show n))


-- Gets all digits, then convert them from ascii to integer

convertAsciiToIntegerList [] = []
convertAsciiToIntegerList (x:xs) = ((fromEnum x)-48) : convertAsciiToIntegerList xs

--First we get integer List of digits though n

getSumOfDigitSquares n = sum (map (^2) (convertAsciiToIntegerList (show n)))


--if not 1 or 89 then recuricely call with the nuext number in the series
--which can be determined by getSumOfDigitsSquares

endsIn89 n
 | n == 89 = True
 | n == 1 = False
 | otherwise = endsIn89 (getSumOfDigitSquares n)

--Give a number of numbers, count how many of them converge to 89

countEndsIn89 n = length [x | x <- [1..n], endsIn89 x]


--Sum of consecutive Squares
isSumOfConsecutiveSquares n s = isocs n s 0
 where 
 isocs n s t 
    | t == n = True 
    | t>n = False
    | otherwise = isocs n (s+1)(t+(s*s))

listEm n s = [x | x <- [1..n], isPalindrome x, isSumOfConsecutiveSquares x s]


isqrt :: Integral i => i -> i
isqrt = floor.sqrt.fromIntegral

  
