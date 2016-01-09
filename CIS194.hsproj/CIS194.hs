--Exersize 1 - Credit Card Vaildation

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer

toDigits x
  | x <= 0 = []
  | otherwise = (toDigits (x `div` 10)) ++ [(x `mod` 10)]

toDigitsRev x = reverse (toDigits x)

doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = 
  x : y*2 : doubleEveryOther zs

doubleEveryOther x = (reverse (doubleEveryOtherRev (reverse x)))

sumDigits [] = 0
sumDigits (x:xs)  
  |x >= 10 = sumDigits ((x `div` 10) : (x `mod` 10): []) + (sumDigits xs)
  |otherwise = x + sumDigits xs
