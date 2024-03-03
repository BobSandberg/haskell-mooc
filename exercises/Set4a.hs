-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array
import Set3a (capitalize)
import Text.XHtml (base, background)

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
-- allEqual :: [a] -> Bool
allEqual [] = True
allEqual [h] = True
allEqual (h1:h2:xs) = h1 == h2 && allEqual (h2:xs)


------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True


-- distinct :: Eq a => [a] -> Bool
-- distinct [] = True
-- distinct [h] = True
-- distinct l = distinct_ l (tail l)

-- distinct_ :: Eq a => [a] -> [a] -> Bool
-- distinct_ [] [] = True
-- distinct_ [h] [] = True
-- distinct_ (h:t) [] = distinct_ t (tail t)
-- distinct_ (h1:t1) (h2:t2) 
--     | h1 == h2      = False
--     | otherwise     = distinct_ (h1:t1) t2

distinct :: Ord a => [a] -> Bool
distinct = distinctOrdered . sort

distinctOrdered :: Eq a => [a] -> Bool
distinctOrdered [] = True
distinctOrdered [h] = True
distinctOrdered (h1:h2:t)
    | h1 == h2  = False
    | otherwise = distinctOrdered (h2:t)

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
-- middle a b c = head . tail . sort $ [a,b,c]

middle a b c = sort [a,b,c] !! 1

-- middle a b c
--     | b <= a && a <= c || c <= a && a <= b      = a
--     | a <= b && b <= c || c <= b && b <= a      = b
--     | otherwise                                 = c


------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Ord a, Num a) => [a] -> a
rangeOf xs = let h = head xs
             in fst (maxminRange xs (h,h))

maxminRange :: (Ord a, Num a) => [a] -> (a,a) -> (a,(a,a))
maxminRange []    (mx,mn) = (mx - mn, (mx, mn))
maxminRange (h:t) (mx,mn) = maxminRange t (max h mx, min h mn)

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

longest :: Ord a => [[a]] -> [a]
longest lol = let key l = (-1 * length l, head l)
                  keyval l = (key l, l)
              in snd . minimum . map keyval $ lol

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey k kvs = let incOnMatch mk (k,v) 
                            | mk == k   = (k, v+1)
                            | otherwise = (k,v)
                     in map (incOnMatch k) kvs


------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: (Foldable t, Fractional a) => t a -> a
average xs = foldr (+) 0 xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

winner :: Map.Map String Int -> String -> String -> (String,Int)
winner scores player1 player2 = let ms1 = Map.lookup player1 scores 
                                    ms2 = Map.lookup player2 scores 
                                in case (ms1,ms2) of
                                    (Just s1, Just s2)  
                                        | s1 >= s2  -> (player1,s1)
                                        | otherwise -> (player2,s2)
                                    (Just s1, Nothing) -> (player1,s1)
                                    (Nothing, Just s2) -> (player2,s2)
                                    (Nothing, Nothing) -> (player1,0)


------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs = foldr incCount Map.empty

incCount :: (Ord k, Eq k) => k -> Map.Map k Int -> Map.Map k Int
incCount k m = case Map.lookup k m of 
                    Nothing   -> Map.insert k 1 m
                    Just c    -> Map.adjust (+1) k m

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank = 
    case ( Map.lookup from bank, Map.lookup to bank ) of
        (_, Nothing)                -> bank
        (Nothing, _)                -> bank
        ( Just fbal, Just tbal )    -> if fbal - amount >= 0 
                                       then transfer_ from to amount bank
                                       else bank


transfer_ :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer_ fromAcct toAcct amount bank = 
        let adjust = Map.adjust
        in adjust (+ amount) toAcct . adjust (flip (-) amount) fromAcct $ bank


------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = let iv = arr ! i
                   jv = arr ! j
                in arr // [(i,jv), (j,iv)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs

-- maxIndex :: (Ix i, Ord a, Enum a) => Array i a -> i
-- maxIndex arr = let l = assocs arr
--                in fst (foldr (\mx el -> if snd mx > snd el then mx else el) (head l) (tail l))

-- maxIndex :: (Ix i, Ord a, Enum a) => Array i a -> i
-- maxIndex arr = foldr1 (\i mx -> if arr ! i > arr ! mx then i else mx) (indices arr)

maxIndex :: (Ix i, Ord a, Enum a) => Array i a -> i
maxIndex = let maxValue t1 t2 = if snd t1 > snd t2 then t1 else t2
           in fst . foldr1 maxValue . assocs
