# fibbuzz

To run, 

```
stack build 
stack exec fibbuzz -- <number of Fibonacci numbers to generate>
```

For example, for the first 10 Fibonacci numbers, run `stack exec fibbuzz -- 10`.

Documentation available with `stack haddock`.

# Question
The [question](https://github.com/swift-nav/screening_questions/blob/master/questions.md#swift-navigation-application-questions) is: 

In the programming language of your choice, write a program generating the first n Fibonacci numbers F(n), printing
- "Buzz" when F(n) is divisible by 3.
- "Fizz" when F(n) is divisible by 5.
- "FizzBuzz" when F(n) is divisible by 15.
- "BuzzFizz" when F(n) is prime.
- the value F(n) otherwise.



