import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- A qucick revision before we start.
-- 0. We are given a function that takes a Char and returns a bool, depending on:
-- If the char is digit between 1 and 7 - True
-- In all other cases False
returnSertainValue:: Char -> Bool 
returnSertainValue l 
					|isDigit l = ((digitToInt  l) > 1) && ((digitToInt l) < 7) 
					|otherwise = False 

-- 1. We are given a function that takes a Char and a Int, and reurns an Int:
-- if the Char is a Digit, we return 100 * our Int
-- if the Char is a Space, we return 10 * our Int
-- if the Char is a Letter we return our Int
-- in all other cases, we retrun 0
returnSertainValueSecond:: Char -> Int -> Int 
returnSertainValueSecond l n
							| isDigit l = 100 * n 
							| isSpace l = 10 * n 
							| isLetter l = n 
							| otherwise = 0

-- 2. Our function takes a String and we need to return the number of spaces in it
returnNumeberOfSpaces:: String -> Int 
returnNumeberOfSpaces w = length [x | x <- w, isSpace x]

-- 3. Write a function that counts how many capital letters are in a string
countCapitalLetters:: String -> Int 
countCapitalLetters t = length [x | x <- t, isUpper x]

-- Before we see recursion we should check out pattern matching
-- 1. Lets write a function that returns a string depending on an int input
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
---- we are missing a copule of matches, can you fill them in
sayMe 3 = "Tree"
sayMe 4 = "Four"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- 2. This function takes a character and returns a name starting with that character
charName :: Char -> String
charName 'K' = "Kami"
---- Lest write a couple of more matches
charName 'M' = "Mama"
charName 'R' = "Rose"
charName 'N' = "Name"
charName а = "John Doe"
---- And if none of the matches match - we should return "John Doe"

-- 3. Now lets match a tuple - our function gets a (Int, Int) and needs to return the product of the two 
-- elements of the tuple:
multiple2DTuple :: (Int, Int) -> Int
multiple2DTuple (x, y) = x * y

-- 4. Now lets do the same for a (Int, Int, Int) tuple:
multiple3DTuple :: (Int, Int, Int) -> Int
multiple3DTuple (x, y, z) = x * y * z

-- 5. And what about a 4D tuple?
multiple4DTuple :: (Num a) => (a, a, a, a) -> a
multiple4DTuple (x, y, z, o) = x * y * z * o

-- 6. And what about 5D.. ok just kidning, let move on :D
-- Now our function is given 2 vetors with 2 components, and we need to sum all their components

sumVectorComponents :: (Int, Int) -> (Int, Int) -> Int
sumVectorComponents (x, y) (u, v) = x + u + y + v

findDotProduct:: (Int, Int, Int) -> (Int, Int, Int) -> Int 
findDotProduct (x, y, z) (u, s, t) = x * u + y * s + z * t 

-- 7. And what about if we want to multiply them
multiplyVectorComponents :: (Int, Int) -> (Int, Int) -> Int
multiplyVectorComponents (x, y) (u, v) = x * u * y * v

-- 8. Now lets sum the 2 vectors and return another vector
-- Can you help me finish it
sumVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumVectors (x, y) (u, s) = (x + u, y + s)

-- Now lest match some lists. Example
tell :: [Int] -> String
tell [] = "The list is empty"
-- tell ([x]) = 
-- tell ([x,y]) = 
-- tell ([x,y,_]) =
tell (x:[]) = "The list has one element : " ++ show x
tell (x:y:[]) = "The list has two elements : " ++ show x ++ " and " ++ show y
tell (x:y:_) = " This list is long. The first two elements are : " ++ show x ++ " and " ++ show y

-- 9. We need to write a function that takes a list of Ints and returns a String and:
-- if it has only one element -"Just one"
-- if it has only two elements - "Just two"
-- if the input list is empty - "None"
-- in all other cases - "There are more that tree elements"
evaluateList :: [Int] -> String 
evaluateList [] = "None"
evaluateList ([x]) = "Just one"
evaluateList ([x, y]) = "Just two"
evaluateList _ = "There are more that tree elements"


-- 10. write a function that takes a String and returns a string:
-- if the string is empty - throw an error
-- if else - return the same string
operationsWithString :: String -> String 
operationsWithString "" = error "Empty String"
operationsWithString word = word


-- Recursion gives us a way to filter and modify list (and more complex solutions).
-- Recursion must always have an end case (a bottom), where it should stop
-- lets look at a qucik example:

-- increase all the elements of a list with 10
increaseElementsRec :: [Int] -> [Int]
increaseElementsRec [] = []
increaseElementsRec (x:xs) =  (x + 10) : increaseElementsRec xs

increaseElements :: [Int] -> [Int]
increaseElements n = [x + 10 | x <- n]

testCheck:: [Int] -> Bool 
testCheck xs  = increaseElementsRec xs == increaseElements xs