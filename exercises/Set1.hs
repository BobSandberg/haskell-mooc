-- Welcome to the first exercise set of the Haskell Mooc! Edit this
-- file according to the instructions, and check your answers with
--
--   stack runhaskell Set1Test.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set1.hs
--
-- This set contains exercises on
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

module Set1 where

import Mooc.Todo
import GHC.Real (infinity)

------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.

one :: Int
one  = 1

two :: Int
two = 2

------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = 2*x

-- To verify
-- ghci> double 1
-- 2
-- ghci> double 10
-- 20

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
-- quadruple x = double (double x)
quadruple = double . double -- Is this cheating?

-- To verify
-- ghci> quadruple 20
-- 80

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).
--
-- Give distance a type signature, i.e. distance :: something.
--
-- PS. if you can't remember how the distance is computed, the formula is:
--   square root of ((x distance) squared + (y distance) squared)
--
-- Examples:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = let dx = x2 - x1
                           dy = y2 - y1
                       in sqrt( dx^2 + dy^2 )

-- To verify
-- ghci> distance 0 0 1 1
-- 1.4142135623730951
-- ghci> distance 1 1 4 5
-- 5.0
-- ghci> distance 0 0  3 4
-- 5.0
-- ghci> distance 0 0  5 12
-- 13.0

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
-- eeny n = if even n then "eeny" else "meeny"
eeny n = let myEven n = n `mod` 2 == 0
         in if myEven n then "eeny" else "meeny"

-- To verify
-- [eeny n | n <- [2..50]]

------------------------------------------------------------------------------
-- Ex 6: here's the function checkPassword from the course material.
-- Modify it so that it accepts two passwords, "swordfish" and
-- "mellon".

-- checkPassword :: String -> String
-- checkPassword password = if password == "swordfish"
--                          then "You're in."
--                          else if password == "mellon"
--                               then "You're in."
--                               else "ACCESS DENIED!"

checkPassword :: String -> String
checkPassword password 
    | password == "swordfish"   = "You're in."
    | password == "mellon"      = "You're in."
    | otherwise                 = "ACCESS DENIED!"

-- To verify
-- ghci> checkPassword "swordfish"
-- "You're in."
-- ghci> checkPassword "mellon"
-- "You're in."
-- ghci> checkPassword "anything else"
-- "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: A postal service prices packages the following way.
-- Packages that weigh up to 500 grams cost 250 credits.
-- Packages over 500 grams cost 300 credit + 1 credit per gram.
-- Packages over 5000 grams cost a constant 6000 credits.
--
-- Write a function postagePrice that takes the weight of a package
-- in grams, and returns the cost in credits.

postagePrice :: Int -> Int
postagePrice g | g <= 500  = 250
               | g <= 5000 = 300 + g
               | g >  5000 = 6000

-- To verify
-- ghci> postagePrice 0
-- 250
-- ghci> postagePrice 500
-- 250
-- ghci> postagePrice 501
-- 801
-- ghci> postagePrice 5000
-- 5300
-- ghci> postagePrice 5001
-- 6000
-- ghci> postagePrice 6000
-- 6000

------------------------------------------------------------------------------
-- Ex 8: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. remember, the type of booleans in haskell is Bool

isZero :: Integer -> Bool
isZero n = n == 0  

isZero2 :: Integer -> Bool
isZero2 0 = True
isZero2 _ = False


-- To verify
-- ghci> isZero (-1)
-- False
-- ghci> isZero 3
-- False
-- ghci> isZero 0
-- True

------------------------------------------------------------------------------
-- Ex 9: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo n | n < 0  = n `div` 0
        | n == 0 = 0
        | n > 0  = n + sumTo (n-1)

-- To verify
-- ghci> sumTo 0
-- 0
-- ghci> sumTo 1
-- 1
-- ghci> sumTo 10
-- 55
-- ghci> 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
-- 55
-- ghci> sumTo (-1)
-- *** Exception: divide by zero

------------------------------------------------------------------------------
-- Ex 10: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer

power _ 0 = 1
power n m = n * power n (m-1)

-- To verify
-- ghci> power 10 0
-- 1
-- ghci> power 10 1
-- 10
-- ghci> power 10 2
-- 100
-- ghci> power 10 20
-- 100000000000000000000 <- Count'em. There are 20 0s
-- ghci> power 2 2
-- 4
-- ghci> power 2 8
-- 256
-- ghci> power 2 16
-- 65536
-- ghci> power 2 32
-- 4294967296
-- ghci> (power 2 32) `div` (power 2 16)
-- 65536

------------------------------------------------------------------------------
-- Ex 11: ilog3 n should be the number of times you can divide given
-- number by three (rounding down) before you get 0.
--
-- For example, ilog3 20 ==> 3 since
--   20/3 = 6.66 (gets rounded down to 6)
--   6/3 = 2
--   2/3 = 0.666 (gets rounded down to 0)
--
-- Use recursion to define ilog3. Use the function "div" for integer
-- division. It rounds down for you.
--
-- More examples:
--   ilog3 2 ==> 1
--   ilog3 7 ==> 2

-- ilog3 :: Integer -> Integer
-- ilog3 n = let d :: Double
--               d = fromIntegral n
--           in ilog3' d 0

-- ilog3' :: Double -> Integer -> Integer
-- ilog3' d c | d < 3.0 = c + 1
--            | d >= 3.0 = ilog3' (d / 3.0) (c + 1)

-- To verify (1st version)
-- ghci> ilog3 20
-- 3

-- Lets try again and make an integer divide 

ilog3 :: Integer -> Integer
ilog3 n = ilog3' n 0

ilog3' :: Integer -> Integer -> Integer
ilog3' n c | n == 0     = c
           | otherwise  = ilog3' (n `div` 3) (c + 1)


-- To verify (1st version)
-- ghci> ilog3 20
-- 3
-- ghci> ilog3 60
-- 4
-- ghci> ilog3 120
-- 5
-- ghci> ilog3 121
-- 5
-- ghci> ilog3 360
-- 6
-- ghci> ilog3 359
-- 6

-- Generalize and make it perform like logBase, but instead returns essentially the floor logBase
ilog :: Integer -> Integer -> Integer
ilog b n = ilog' b n (-1)

ilog' :: Integer -> Integer -> Integer -> Integer
ilog' b n c | n == 0 && c < 0 = floor (1/0)
            | n == 0          = c
            | otherwise       = ilog' b (n `div` b) (c + 1)


-- To verify (1st version)
-- ghci> ilog 3 0
-- 179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216-- ghci> ilog 3 1
-- 0
-- ghci> ilog 3 3
-- 1
-- ghci> ilog 3 (3^2)
-- 2
-- ghci> ilog 3 (3^3)
-- 3
-- ghci> ilog 3 (3^4)
-- 4
-- ghci> ilog 3 (3^4-1)
-- 3
-- ghci> ilog 3 (3^20)
-- 20
-- ghci> ilog 3 (3^20-1)
-- 19
