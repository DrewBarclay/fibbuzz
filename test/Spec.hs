module Main where

import Test.Hspec
import Test.QuickCheck
import Lib(fibs, buzz, isPrime)

numFibs = 10000

main :: IO ()
main = hspec $ do
  describe "Fibonacci Numbers" $ do
    it "Numbers equal to the sum of the last two" $ lastTwoSum $ take numFibs fibs
    it "First 10 equal 0, 1, 1, 2, 3, 5, 8, 13, 21, 34" $ take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    it "15 prints FizzBuzz" $ buzz 15 `shouldBe` "FizzBuzz"
    it "6 prints Buzz" $ buzz 6 `shouldBe` "Buzz"
    it "10 prints Fizz" $ buzz 10 `shouldBe` "Fizz" 
    it "7 prints BuzzFizz" $ buzz 7 `shouldBe` "BuzzFizz"
    it "8 prints 8" $ buzz 8 `shouldBe` "8"
  describe "isPrime" $ do
    it "1 is not prime" $ isPrime 1 `shouldBe` False 
    it "2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 all prime" $ all isPrime [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]
    it "Two numbers > 1 multiplied together are not prime" $ property prop_TwoMultNotPrime

lastTwoSum :: [Integer] -> Bool
lastTwoSum (x:x2:x3:xs) = (x + x2 == x3) && lastTwoSum xs
lastTwoSum _ = True

prop_TwoMultNotPrime n m = (n > 1 && m > 1) ==> not $ isPrime (n * m)