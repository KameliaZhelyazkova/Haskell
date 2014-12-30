import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-----------------------------------------------------
---    			Higher Order Functions			  ---
-----------------------------------------------------

-- Basicly we are going to look at 3 functions - filter, map, foldr. Plus we are going to have a look at lambdas

-- Filter - this functions is used... well for filtering.
-- Its type signature is:
-- filter:: (a -> Bool) -> [a] -> [a]
-- Or it takes:
-- - (a -> Bool) a function that takes an element of type "a" (generic type) and returns a bool.
-- so this is like the predicate(filter function) in list comprehension.
-- - [a] - it takes a list of elements of type "a"
-- - it returns a list of type [a]
-- Note: >, <, >=, ==, <= - are all functions that return a bool
-- Here are a few examples
-- 1. Take from a list only the positive numbers
takePostivieNumbers :: [Int] -> [Int]
takePostivieNumbers n = filter (>0) n

-- 2. Take only the numbers that can be divided by 3
checkIfCanBeDividedbBy3 :: Int -> Bool
checkIfCanBeDividedbBy3 n = n `mod` 3 == 0

takeDividebaleBy3 :: [Int] -> [Int]
takeDividebaleBy3 n = filter (checkIfCanBeDividedbBy3) n

-- Map - this function modifys all the elements of a list
-- Its type signature is: 
-- (a -> b) -> [a] -> [b]
-- it takes a list of type [a] and for every element it applies the function (a -> b), 
-- that transform the list into a list of type [b]
-- Lets look at a couple of examples:
-- 3. Increase the value of all elements by 3
increaseBy3 :: [Int] -> [Int]
increaseBy3 n = map (+3) n

-- 4. transform a list of Ints to a list of Strings
-- [1,2,3,4] -> ["1", "2", "3", "4"]
transformToString :: [Int] -> [String]
transformToString n = map (show) n

-- Foldr - it is used for aggregating a list of values into one value
-- Lst directly look at some examples here:

-- 5. Sum all the values of a list
sumList :: [Int] -> Int
sumList n = foldr (+) 0 n

-- 6. Check if all the values in a list of Bools are True
checkIfAllTrue :: [Bool] -> Bool
checkIfAllTrue n = foldr (&&) True n