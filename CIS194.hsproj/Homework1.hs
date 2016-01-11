--Exersize 1 - Credit Card Vaildation
module Homework1 where

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool
  
toDigits x
  | x <= 0 = []
  | otherwise = (toDigits (x `div` 10)) ++ [(x `mod` 10)]
  
toDigitsRev x = reverse (toDigits x)
  
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = 
  x : y*2 : doubleEveryOtherRev (zs)
  
doubleEveryOther x = (reverse (doubleEveryOtherRev (reverse x)))
  
sumDigits [] = 0
sumDigits (x:xs)  
  |x >= 10 = sumDigits ((x `div` 10) : (x `mod` 10): []) + (sumDigits xs)
  |otherwise = x + sumDigits xs
  
validate x =
  ((sumDigits (doubleEveryOther (toDigits x))) `mod` 10) == 0

