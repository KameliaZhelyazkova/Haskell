import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- Before we start lets do a quick revision:

-- 0. Check if a number is between 14 and 18

-- 1. We are given a Char. Determen if the char is a digit and if it is in the 5 to 9 interval (return a Bool)
isTheCharDigit :: Char -> Bool 
--isTheCharDigit l = isDigit l && ((digitToInt l) > 5) && ((digitToInt l) < 9)
isTheCharDigit l 
				| isDigit l = ((digitToInt l) > 5) && ((digitToInt l) < 9)  
				| otherwise = False

-- 2. Witre a function that takes char and:
-- if the Char is a number return 4 duplictes of it in form of a string. Example: -> '9' -> "9999"
-- if the Char is a letter return 7 duplicates of it in form of a string. Example: -> 'a' -> "aaaaaaa"
-- if the Char is a space - return an empty string
-- in all other cases throw an error
returnCertainOutput:: Char -> String 
returnCertainOutput l 
					| isDigit l = replicate 4 l 
					| isLetter l = replicate 7 l
					| isSpace l = ""
					| otherwise = error "Not covered in the required cases"


-- 3. We are given a Char, we should return:
-- if the char amoung 'P', 'K', 'T' we shoudl return a string, starting with the coresponding letter
-- in all other cases we should return "This letter is different"
-- Please use pattern matching
chooseName:: Char -> String 
chooseName 'P' = "P"
chooseName 'K' = "K"
chooseName 'T' = "T"
chooseName a = "This letter is different"

-- 4. We are given a list of Ints, we need to find the count all even nmbers in the list
-- Write a list comprehension and recursion versions of this functions
countEvenNumbers:: [Int] -> Int
countEvenNumbers n = length [x | x <- n, even x]

--Rec 
countEvenNumbersRec:: [Int] -> Int
countEvenNumbersRec [] = 0
countEvenNumbersRec (x:xs) 
						| even x = 1 + countEvenNumbersRec xs
						| otherwise = countEvenNumbersRec  xs

test1:: [Int] -> Bool 
test1 xs = countEvenNumbers xs == countEvenNumbersRec xs

-- 5. Our function should find the sum of all Digits in a String. For example:
-- "S4amp3l" -> 7
-- "Kami08144573" -> 3
-- Write a list comprehension and recursion versions of this function
sumDigits :: String -> Int 
sumDigits t = sum [digitToInt x | x <- t, isDigit x]

--Rec 
sumDigitsRec:: String -> Int 
sumDigitsRec [] = 0
sumDigitsRec (x:xs)
				| isDigit x = digitToInt x + sumDigitsRec xs
				| otherwise = sumDigitsRec xs

test2:: String -> Bool 
test2 xs = sumDigits xs == sumDigitsRec xs

-- 6. We are given a string. Count all the letters in that string
-- Write a list comprehension and recursion versions of this function
countLetters :: String -> Int 
countLetters t = length [x | x <- t, isLetter x]

--Rec 
countLettersRec:: String -> Int 
countLettersRec [] = 0
countLettersRec (x:xs)
					| isLetter x = 1 + countLettersRec xs 
					| otherwise = countLettersRec xs 

test3 :: String -> Bool
test3 xs = countLetters xs == countLettersRec xs 

-- 7. We are given a string. Count all the spaces in that string
-- Write a list comprehension and recursion versions of this function
countSpacesInAString :: String -> Int 
countSpacesInAString l = length [x | x <- l, isSpace x]

-- 8. Remove all the sapces from a word
-- Write a list comprehension and recursion versions of this function
removeSpaces:: String -> String
removeSpaces t = [x | x <- t, not (isSpace x)]

--Rec 
removeSpacesRec:: String -> String 
removeSpacesRec [] = []
removeSpacesRec (x:xs)
					| not (isSpace x) = x : removeSpacesRec xs
					| otherwise = removeSpacesRec xs 

test4 :: String -> Bool 
test4 xs = removeSpaces xs == removeSpacesRec xs

-- 9. Remove all the Letters form a word
-- Write a list comprehension and recursion versions of this function
removeLetters::  String -> String 
removeLetters l = [ x | x <- l, not (isLetter x)]

removeLettersRec :: String -> String
removeLettersRec [] = []
removeLettersRec (x:xs)
						| not (isLetter x) = x : removeLettersRec xs
						| otherwise = removeLettersRec xs 

test5 :: String -> Bool 
test5 xs = removeLetters xs == removeLettersRec xs

-- 10. We are give a string - take all the chars that are on even positions and form a new string
-- Write a list comprehension and recursion versions of this function
formANewString :: String -> String
formANewString inputT = [x | (x,y) <- zip inputT [0..], even y]

--Rec -- 10. We are give a string - take all the chars that are on even positions and form a new string
-- Write a list comprehension and recursion versions of this function
formANewStringRec :: String -> String 
formANewStringRec [] = []
formANewStringRec [a] = []
formANewStringRec (x:y:ys) = x : formANewStringRec ys

-- 11. We are given a string - take all the chars that are on odd positions and form a new string
-- Write a list comprehension and recursion versions of this function
takeFromOddPossition:: String -> String 
takeFromOddPossition inputT = [x | (x,y) <- zip inputT [0..], odd y] 

takeFromOddPossitionRec:: String -> String 
takeFromOddPossitionRec [] = []
takeFromOddPossitionRec [a] = []
takeFromOddPossitionRec (x:y:ys) = y : takeFromOddPossitionRec ys

-----------------------------------------------------------------
----          Polymorphic functions and typeclasses			-----
-----------------------------------------------------------------

-- Basically polymorphic functions are functions that take "a" parameters (generic parameters)
-- that can be of any type - Int, Real, Float, Bool etc.
-- For example lets look at a function that takes a list of "a" elements and duplicates it
-- [1,2,3] -> [1,2,3,1,2,3]
-- ['a', 'b', 'c'] -> ['a', 'b', 'c', 'a', 'b', 'c']

duplicateList :: [a] -> [a]
duplicateList misteryList = misteryList ++ misteryList

-- Our functions does not need to know what the list consists of, just need to duplicate it

-- But what about if we want to access some specific behaviour of our elements?
-- if we what to be able to compare them? Or to garantie that they are number?
-- In this case we can use the so called type classes. Lets look at a few of them

-- (Eq) typeclass - defines that the generic type supports comparison with ==
compareElements :: (Eq a) => a -> a -> Bool
compareElements firstElem secondElem = firstElem == secondElem

-- (Num) typeclass - defines that the generic type can act like a number
multiplyNumbers :: (Num a) => [a] -> a
multiplyNumbers inputList = product inputList

-- (Integral) - similar to (Num) but it includes only whole numbers (Int, Integer)
multiplyIntegralNumbers :: (Integral a) => [a] -> a
multiplyIntegralNumbers inputList = product inputList

-- (Floating) - similar to (Num) but it includes only floating point numbers numbers (Float, Double)
multiplyFloatingNumbers :: (Floating a) => [a] -> a
multiplyFloatingNumbers inputList = product inputList

-- (Show) - defines that a generic type can be converted to string
sayMyName :: (Show a) => a -> String
sayMyName s = show s

-- 1. Now try making the functions form example 0 and 4 polymorphic
-- 0. Check if a number is between 14 and 18
checkIfInInterval:: (Num a) => (Ord a) => a -> Bool 
checkIfInInterval n =  n > 14 && n < 18 

-- 4. We are given a list of Ints, we need to find the count all even nmbers in the list
findEvenNumbers:: (Integral a) => (Ord a) => [a] -> Int
findEvenNumbers n = length [x | x <- n, even x]


-- 7. We are given a string. Count all the spaces in that string
-- Write a list comprehension and recursion versions of this function
--countSpaces:: String => [a] -> Int
--countSpaces l = length [ x | x <- l, isSpace x]

andTwoValues :: Bool -> Bool -> Bool
andTwoValues first second = first && second

orTwoValues :: Bool -> Bool -> Bool
orTwoValues first second = first || second

-- Check if all the letters of a string are capital
checkIfCapital:: String -> Bool 
checkIfCapital t = and [isUpper x | x <- t]

-- Check if there is at least 1 capital letter in a string
checkIfThereIsCapitalLetter:: String -> Bool 
checkIfThereIsCapitalLetter t = or [isUpper x | x <- t]
