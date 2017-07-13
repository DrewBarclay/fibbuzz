{- In the programming language of your choice, write a program generating the first n Fibonacci numbers F(n), printing

    "Buzz" when F(n) is divisible by 3.
    "Fizz" when F(n) is divisible by 5.
    "FizzBuzz" when F(n) is divisible by 15.
    "BuzzFizz" when F(n) is prime.
    the value F(n) otherwise.
-}

module Main where

import Lib(fibs, buzz, isPrime)

import System.Environment
import Control.Monad
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn "Unexpected argument(s) given. Usage: fibbuzz <number of fibonacci numbers>"
    exitFailure

  let n = read (head args) :: Int
  mapM_ (putStrLn . buzz) (take n fibs) --Go over all the fib numbers, print out something depending on their value

