lucky :: (Integral a) => a -> String
lucky 7 = "This is your number 7"
lucky x = "Nope nerdo"


--factorial n = product[1..n]
--not both

-- we must add our basecase
--important to specify the the most specific patterns first and more general ones later

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n*factorial(n-1)

--Prelude> :t 'a'
--'a' :: Char
--Prelude> :t True
--True :: Bool

head' :: [a] -> a
head' [] = error "Empty List, we cant find a head"
head' (x:xs) = x

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--When writing our own functions, we can choose to give them a explicit type declaration
--

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]


maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

--return type is the last value in the delcartion, first values are parameters.
--everything before => is a class constraint
-- :: explicit type annotation

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 


