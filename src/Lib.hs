module Lib (fibs, buzz, isPrime) where

-- |An infinite list of fibonacci numbers. Uses big ints, as fibonacci numbers grow large extremely fast.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) --Perhaps too clever? We start with [0, 1] and add the last two elements in the list to get the next element. As it's lazy and is just doing addition, it's super efficient, though we might want to move to vectors over lists in performance-critical code.

{- |
  Provides the string we wish to output for a given number. 
  * "Buzz" when F(n) is divisible by 3.
  * "Fizz" when F(n) is divisible by 5.
  * "FizzBuzz" when F(n) is divisible by 15.
  * "BuzzFizz" when F(n) is prime.
  * the value F(n) otherwise.
  Note: any n > 1 divides 0, so it is treated as "FizzBuzz". The question is slightly unclear on whether this should be the expected behavior.
-}
buzz :: Integer -> String
buzz n 
  | n `mod` 15 == 0 = "FizzBuzz"
  | isPrime n = "BuzzFizz"
  | n `mod` 3 == 0 = "Buzz"
  | n `mod` 5 == 0 = "Fizz"
  | otherwise = show n

-- |Checks whether a number is prime. Inefficient, written for clarity. Code requiring speed should replace with a more efficient version which eg. generates a list of primes and checks division by those.
isPrime :: Integer -> Bool
isPrime n 
  | n < 2 = False
  | n == 2 = True --Case added because the next line would say 2 is not prime
  | otherwise = not $ any (\m -> n `mod` m == 0) [2..highestFactorToCheck]
  where
    highestFactorToCheck = ceiling . sqrt $ fromIntegral n --No need to check beyond this, if a number higher than this divides n, then the other factors have to be smaller and we would have found them already. Ceiling rather than floor in case of floating point strangeness.