import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- a Quick review of comparing

-- 0. Our function takes an Int and returns a Bool
-- if the input is between 0 and 100 - True
-- all other cases - False
checkInterval :: Int -> Bool 
checkInterval n = 0 < n && n < 100 

-- 1. Our function takes a Char input returns a Bool
-- True if it is one of the following - 'A', 'B', 'Z'
-- False in all other cases
checkSecondInterval :: Char -> Bool 
checkSecondInterval l = l == 'A' || l == 'B' || l == 'Z'

-- 2. Our function takes a Char input returns a Bool
-- True if it is one of the following - 'A', 'B', 'Z', 'a', 'b', 'z'
-- False in all other cases
checkThirdInterval :: Char -> Bool 
checkThirdInterval l = checkSecondInterval (toUpper l)

-- -> 'y'
-- toLower :: Char -> Char

-- 3. Our function takes a Char and retunts an Int based on the following:
-- if the Char is a digit between 1 and 9 (inclusive) - the value of the digit multipled by 7
-- if the Char is the digit 0 - it returns a 0
-- if the Char is a Space - returns -1
-- if the Char is a Letter - returns 100
-- in all other cases it returns -100
checkFourthInterval:: Char -> Int 
checkFourthInterval l 
					| isDigit l && ((digitToInt l) > 1) && ((digitToInt l) <= 9) = (digitToInt l) * 7
					| isDigit l && digitToInt l == 0 = 0
					-- | l == '0' = 0
					| isSpace l = -1 
					| isLetter l = 100
					| otherwise = - 100


-- List Comprehension

-- Basic structure of a list comprehension:

--[%RETURN% | %GENERATOR% , %FILTER%]

-- List comprehension can evalute all elements of a list, and:
-- filter a list
-- modiffy the elements of a list
-- filter and modify the elements of a list
-- 
-- The proccess flow of a list comprehension is:
-- 1. Gets the value for the current element (it goes through all the elements),
-- commonly it is in this form - (x <- xs) , where XS is the value of the list and X 
-- the new of the currently retrived element of the list - (marked with %GENERATOR%)
-- 2. We evalute if a condition is met (predicate) - it must always return a Bool value (marked with %FILTER%)
-- 3. If the evaluation of the %FILTER% is Trre we can return something. 
-- Commonly modification of our list is done here - marked with 


-- Lets first look how a list comprehension, can be used to filter a list

-- Takes only the numbers that are bigger than 7 from a list
takeBiggerThan7 :: [Int] -> [Int] 
takeBiggerThan7 xs = [x | x <- xs, x > 7]

-- Rec
takeBiggerThan7Rec :: [Int] -> [Int]
takeBiggerThan7Rec [] = []
takeBiggerThan7Rec (x:xs)
					| x > 7 = x : takeBiggerThan7Rec xs
					|otherwise = takeBiggerThan7Rec xs

--Test
test:: [Int] -> Bool 
test xs = takeBiggerThan7 xs == takeBiggerThan7Rec xs

-- Take only the capital letters form a word
takeCapitals :: String -> String
takeCapitals word = [x | x <- word, isUpper x]

--Rec
takeCapitalsRec :: String -> String
takeCapitalsRec [] = []
takeCapitalsRec (x:xs)
						| isUpper x = x : takeCapitalsRec xs
						| otherwise = takeCapitalsRec xs

--Test 
test2 :: String -> Bool 
test2 xs = takeCapitals xs == takeCapitalsRec xs


-- Now lest look and a copule of examples for how a list comprehension can modify a list

-- multiply all the elements of a list by 13
multipleElementsBy13 :: [Int] -> [Int]
multipleElementsBy13 xs = [x * 13 | x <- xs]

--Rec 
multipleElementsBy13Rec :: [Int] -> [Int]
multipleElementsBy13Rec [] = []
multipleElementsBy13Rec (x:xs) = x * 13 : multipleElementsBy13Rec xs

--Test
test3 :: [Int] -> Bool 
test3 xs = multipleElementsBy13 xs == multipleElementsBy13Rec xs


-- replace all the letters 's' with 'S' in a list

replaceLowerS :: String -> String
replaceLowerS word = [if 's' == x then 'S' else x | x <- word]

--Rec - "sale"
replaceLowerSRec :: String -> String
replaceLowerSRec [] = []
replaceLowerSRec (x:xs)
						| x == 's' = 'S' : replaceLowerSRec xs
						| otherwise = x : replaceLowerSRec xs 

--tets 
test4 :: String -> Bool 
test4 xs = replaceLowerS xs == replaceLowerSRec xs 

-- List Comprehension
-- The functions takes a list of Ints and we should take from it only the numbers between 0 and 100
-- Hint: Check the functions we have writen before and see whether we can use something.
returnSertainNumbers:: [Int] -> [Int]
returnSertainNumbers n = [x | x <- n, checkInterval x]
--reternSertainNumbers n = [x | x <- n, x > 0 && x < 100]

--Rec
returnSertainNumbersRec :: [Int] -> [Int]
returnSertainNumbersRec [] = []
returnSertainNumbersRec (x:xs)
							| checkInterval x = x : returnSertainNumbersRec xs
							| otherwise = returnSertainNumbersRec xs

--test 
test5 :: [Int] -> Bool 
test5 xs = returnSertainNumbers xs == returnSertainNumbersRec xs

-- The function should take only the letters 'А', 'B', 'Z' from a string
returnSertainLetters:: String -> String
returnSertainLetters n = [x | x <- n, checkSecondInterval x]

-- Rec 
returnSertainLettersRec :: String -> String
returnSertainLettersRec [] = []
returnSertainLettersRec (x:xs)
								| checkSecondInterval x = x : returnSertainLettersRec xs
								| otherwise = returnSertainLettersRec xs
--								| x == 'A' = x : returnSertainLettersRec xs
--								| x == 'B' = x : returnSertainLettersRec xs
--								| x == 'Z' = x : returnSertainLettersRec xs
--								|otherwise = returnSertainLettersRec xs

-- Test
test6 :: String -> Bool 
test6 xs = returnSertainLetters xs == returnSertainLettersRec xs


-- The function should take only the letters 'А', 'B', 'Z', 'a', 'b', 'z' from a string.
-- Искам да го напиша без помощна функция.

returnSertainChars:: String -> String 
returnSertainChars n = [x | x <- n, checkThirdInterval x]
--returnSertainChars n = [x | x <- n, checkSecondInterval (toUpper x)]

-- Rec
returnSertainCharsRec :: String -> String 
returnSertainCharsRec [] = []
returnSertainCharsRec (x:xs)
							| checkThirdInterval x = x : returnSertainCharsRec xs
							| otherwise = returnSertainCharsRec xs 

--Test 
test7 :: String -> Bool 
test7 xs = returnSertainChars xs == returnSertainCharsRec xs


-- Write a function that multiples all the even elements in a list.
multiplyEvenNumber:: [Int] -> Int
multiplyEvenNumber n = product [x | x <- n, even x]

--Rec 
multiplyEvenNumberRec :: [Int] -> Int
multiplyEvenNumberRec [] = 1 
multiplyEvenNumberRec (x:xs)
							| even x = x * multiplyEvenNumberRec xs
							| otherwise = multiplyEvenNumberRec xs  

-- Test 
test8 :: [Int] -> Bool 
test8 xs =  multiplyEvenNumber xs == multiplyEvenNumberRec xs 


-- write a function that sums all the odd elements in a list
sumOddElements:: [Int] -> Int
sumOddElements n = sum [x | x <- n, odd x]

-- Rec 
sumOddElementsRec :: [Int] -> Int 
sumOddElementsRec [] = 0
sumOddElementsRec (x:xs) 
						| odd x = x + sumOddElementsRec xs
						| otherwise = sumOddElementsRec xs

-- write a function that counts all the postive elements in a list
countPositiveElements:: [Int] -> Int 
countPositiveElements n = length [x | x <- n, x > 0]

-- Rec ------------------------------------ [1, -2, 3, 1, -1]
countPositiveElementsntRec :: [Int] -> Int 
countPositiveElementsntRec [] = 0
countPositiveElementsntRec (x:xs)
								| x > 0 = 1 + countPositiveElementsntRec xs 
								| otherwise = countPositiveElementsntRec xs 

-- write a function that finds the biggest negative element in a list
findBiggestNegativeElement:: [Int] -> Int
findBiggestNegativeElement n = maximum [x | x <- n, x < 0]

--Rec 
findBiggestNegativeElementRec:: [Int] -> Int 
findBiggestNegativeElementRec [a] = a -- и това се явява последен елемент?Ахааа, стигаме до края
findBiggestNegativeElementRec [] = 0
findBiggestNegativeElementRec (x:xs)
									| x < 0 =  x `max` findBiggestNegativeElementRec xs
									| otherwise = findBiggestNegativeElementRec xs

-- write a function that returns only the elements form a list that are divisible by 3 and 7
returnElementsDivisibleByTreeAndSeven:: [Int] -> [Int]
returnElementsDivisibleByTreeAndSeven n = [x | x <- n, mod x 3 == 0 && mod x 7 == 0]

--Rec 
returnElementsDivisibleByTreeAndSevenRec :: [Int] -> [Int]
returnElementsDivisibleByTreeAndSevenRec [] = []
returnElementsDivisibleByTreeAndSevenRec (x:xs)
												| mod x 3 == 0 && mod x 7 == 0 = x : returnElementsDivisibleByTreeAndSevenRec xs
												| otherwise = returnElementsDivisibleByTreeAndSevenRec xs

-- mod takes 2 paramethers, the number you want to divide by and the number by which you want to divide
-- examples
-- mod 3 3 == 0
-- mod 5 2 == 3
-- mod 5 3 == 2
