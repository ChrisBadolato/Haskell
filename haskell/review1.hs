isDivisible number divisor = number `mod` divisor == 0

isDivisibleByList number [] = True

sumOfSquares n = sum[x^2 | x <- [1..n]]

squareOfSums n = (sum[x|x<-[1..n]])^2


difference n = (squareOfSums n)-(sumOfSquares n)