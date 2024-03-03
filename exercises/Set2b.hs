module Set2b where

import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List

------------------------------------------------------------------------------
-- Ex 1: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial _ 0    = 1
binomial 0 k    = 0
binomial n k    = binomial (n-1) k + binomial (n-1) (k-1)

------------------------------------------------------------------------------
-- Ex 2: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

oddFactorial :: Integer -> Integer
oddFactorial n
    | even n    = floor (0/0)
    | otherwise = oddFactorial' n 1

oddFactorial' :: Integer -> Integer -> Integer
oddFactorial' n prev_fact
    | n==1      = prev_fact
    | otherwise = oddFactorial' (n-2) (prev_fact * n)

------------------------------------------------------------------------------
-- Ex 3: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,
-- * if one is zero, return the other number
-- * if not, subtract the smaller number from the larger one
-- * replace the larger number with this new number
-- * repeat
--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:
-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd 0 b = b
myGcd a 0 = a
myGcd a b = myGcd (abs (a-b))  (min a b)

-- myGcd a b = 
--     | a >= b = myGcd (a-b)  b
--     | a <  b = myGcd  a    (b-a)



------------------------------------------------------------------------------
-- Ex 4: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:
-- * you can combine strings with the ++ operator.
-- * you can compute the length of a string with the length function

leftpad :: String -> Int -> String
leftpad s n
    | n < 0     = "ERROR: pad length must be positive"
    | otherwise = let pad = map (const ' ') [1..n]
                  in  pad ++ s

repeatChar :: Int -> Char -> String
repeatChar 0 ch = []
repeatChar n ch = ch : repeatChar (n-1) ch

leftpad2 :: String -> Int -> String
leftpad2 s n
    | n < 0     = "ERROR: pad length must be positive"
    | otherwise = let pad = repeatChar n ' '
                  in pad ++ s


------------------------------------------------------------------------------
-- Ex 5: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:
-- * you can combine strings with the ++ operator
-- * you can use the show function to convert a number into a string
-- * you'll probably need a recursive helper function

countdown :: Integer -> String
countdown n = "Ready! " ++ countdownseq n "" ++ "Liftoff!"

countdownseq :: Integer -> String -> String
countdownseq 0 seq = seq
countdownseq n seq = countdownseq (n-1) (seq ++ show n ++ "... ") 


-- To verify
-- ghci> countdown 10
-- "Ready! 10... 9... 8... 7... 6... 5... 4... 3... 2... 1... Liftoff!"

------------------------------------------------------------------------------
-- Ex 6: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!
--
-- Hint: remember the mod function!

-- I wrote this earlier (I think as part of some Mooc reading)
primes = [ n | n <- [2..] , all (\k -> n `mod` k /= 0) [2..n `div` 2] ]

dividesevenly :: Integer -> Integer -> Bool
dividesevenly n d = (n `mod` d) == 0

smallestDivisor :: Integer -> Integer
smallestDivisor n = let Just firstdivisor = find (dividesevenly n) primes
                    in firstdivisor

-- To verify
-- ghci> map smallestDivisor [2..100]
-- [2,3,2,5,2,7,2,3,2,11,2,13,2,3,2,17,2,19,2,3,2,23,2,5,2,3,2,29,2,31,2,3,2,5,2,37,2,3,2,41,2,43,2,3,2,47,2,7,2,3,2,53,2,5,2,3,2,59,2,61,2,3,2,5,2,67,2,3,2,71,2,73,2,3,2,7,2,79,2,3,2,83,2,5,2,3,2,89,2,7,2,3,2,5,2,97,2,3,2]

------------------------------------------------------------------------------
-- Ex 7: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime 1 = True
isPrime n = n == smallestDivisor n

-- To verify
-- ghci> map (\n -> (n, isPrime n)) [1..100]
--[(1,True),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False),(13,True),(14,False),(15,False),(16,False),(17,True),(18,False),(19,True),(20,False),(21,False),(22,False),(23,True),(24,False),(25,False),(26,False),(27,False),(28,False),(29,True),(30,False),(31,True),(32,False),(33,False),(34,False),(35,False),(36,False),(37,True),(38,False),(39,False),(40,False),(41,True),(42,False),(43,True),(44,False),(45,False),(46,False),(47,True),(48,False),(49,False),(50,False),(51,False),(52,False),(53,True),(54,False),(55,False),(56,False),(57,False),(58,False),(59,True),(60,False),(61,True),(62,False),(63,False),(64,False),(65,False),(66,False),(67,True),(68,False),(69,False),(70,False),(71,True),(72,False),(73,True),(74,False),(75,False),(76,False),(77,False),(78,False),(79,True),(80,False),(81,False),(82,False),(83,True),(84,False),(85,False),(86,False),(87,False),(88,False),(89,True),(90,False),(91,False),(92,False),(93,False),(94,False),(95,False),(96,False),(97,True),(98,False),(99,False),(100,False)]

------------------------------------------------------------------------------
-- Ex 8: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- You don't need to care about arguments less than 2. Any behaviour
-- for them is fine.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n = last( takeWhile (n >=) primes )

biggestPrimeDivisor :: Integer -> Integer
biggestPrimeDivisor n = let primesBelowN = takeWhile (n >=) primes
                       in last (filter (dividesevenly n) primesBelowN)

