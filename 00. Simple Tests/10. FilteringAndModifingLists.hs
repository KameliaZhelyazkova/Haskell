import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- All of the following tasks should be writen in 3 versions:
-- List comprehensive
-- Recursive
-- With Higher Order Functions

----------------------------------------------------------------------------------------------------
--------- 									Filtering									   ---------
----------------------------------------------------------------------------------------------------

-- 1. Get All the positive numbers form a list
-- filter:: (Int -> Bool) -> [Int] -> [Int]
getPositiveNumbers:: [Int] -> [Int]
getPositiveNumbers n = filter (>0) n 

-- 2. Get all the negative numbers form a list
-- filter:: (Int -> Bool) -> [Int] -> [Int]
getNegNumbers:: [Int] -> [Int]
getNegNumbers n = filter (<0) n 

-- 3. Get all the even numbers form a list
-- filter:: (Int -> Bool) -> [Int] -> [Int]
extractEvenNumbers:: [Int] -> [Int]
extractEvenNumbers n = filter (even) n 

-- 4. Get all the odd numbers form a list
extractOddNumbers:: [Int] -> [Int]
extractOddNumbers n = filter (odd) n 

-- By definition Higher Order Functions are functions that take functions as paramathers
-- Example:
-- even:: Int -> Bool
-- odd:: Int -> Bool
-- mod:: Int -> Int -> Int

-- `even` `odd` (odd)

doFunc :: (Int -> Bool) -> Bool
doFunc boolFunc = (boolFunc 12)


-- 5. Get all the numbers form a list that are divisibale by 7
extractDivisibleBy7 :: [Int] -> [Int]
extractDivisibleBy7 n = filter (\x -> mod 7 x == 0) n

--with helper funct.
checkIfDivisibleBy7:: Int -> Bool 
--checkIfDivisibleBy7 n = mod n 7 == 0
checkIfDivisibleBy7 n = n `mod` 7 == 0

-- 6. Get all the numbers form a list that are divisibale by 7 and 3 at the same time
getDivElBy7And3:: [Int] -> [Int]
getDivElBy7And3 n = filter (\x -> mod x 3 == 0 && mod x 7 == 0) n

-- 7. Get all the Capital letters form a string
getCapLetters:: String -> [Char]
getCapLetters n = filter (isUpper) n

-- 8. Get all the numbers for a string
getNumbers:: String -> String
getNumbers n = filter (isDigit) n

-- 9. Remove all the spaces from a string
removeSpaces:: String -> String 
removeSpaces n = filter (\x -> not (isSpace x)) n
--removeSpaces n = [x | x <- n, not (isSpace x)]
checkIfNotSpace:: String -> [Bool]
checkIfNotSpace n = map (\x -> not (isSpace x)) n --Връща лист от Booleans 

-- 9.1. Check if a word has any spaces - if it does - return false else true
areThereSpacesHere:: String -> Bool 
areThereSpacesHere n = and (checkIfNotSpace n)

-- 10. Remove all the numbers form a string
removeAllNumbers:: String -> String 
removeAllNumbers n = filter  (\x -> not (isDigit x)) n 

-- 11. Get only the characters from a string that are 'a', 'b', 'y' or 'z'
-- Example: "The abc soup is very good" -> "aby" 
getCharacters:: [Char] -> String
getCharacters n = filter (\x -> (x == 'a' || x == 'b' || x == 'y' || x == 'z')) n

-- 12. Rewrite 11 but now it should work for 'a', 'b', 'y', 'z', 'A', 'B', 'Y' or 'Z'
-- Example: "Abc is a very good good soup" -> "Abay"
getCharavtersSecond:: [Char] -> String
getCharavtersSecond l = getCharacters l ++ filter (\x -> (x == 'A' || x == 'B' || x == 'Y' || x == 'z')) l

getCharavtersSecondSubpart:: [Char] -> String 
getCharavtersSecondSubpart l = filter (\x -> ((toUpper x) == 'A') || ((toUpper x) == 'B') || ((toUpper x) == 'Y') || ((toUpper x) == 'Z')) l


-- 13. Get all the char from a string, that have and even ASCII number
-- Note: You can get ASCII number with the "ord" function. It gets a char and returns an Int representing, the ASCII code
-- Example:
-- ord 'a' == 97
-- ord 'b' == 98
-- ord 'c' == 99
-- ord '!' == 33

-- 14. Get all the numbers form a list that are between 10 and 100 (inclusive)
getNumbersTenToAHundred:: [Int] -> [Int]
getNumbersTenToAHundred n = [x | x <- n, x > 10 && x <= 100]

getCertainNumbersRec:: [Int] -> [Int]
getCertainNumbersRec [] = []
getCertainNumbersRec (x:xs)
							| x > 10 && x <= 100 = x : getCertainNumbersRec xs
							|otherwise = getCertainNumbersRec xs

getCertainNumbersHOF:: [Int] -> [Int]
getCertainNumbersHOF n = filter (\x -> x > 10 && x <= 100) n

test2 :: [Int] -> Bool 
test2 xs = getNumbersTenToAHundred xs == getCertainNumbersRec xs && getCertainNumbersRec xs == getCertainNumbersHOF xs

-- Sum all numbers in even positions 
sumEvenNumbers:: [Int] -> Int
sumEvenNumbers n = sum [ y | (x,y) <- zip [0..] n, even x]

-- Exam 2012 August--
--2a-- 

--- [3, 2, 9, 7 , 1, -33, -123]
--- [2*3, 7*3]
--- []
p :: [Int] -> Int
p n =  product [ y * 3 | (x,y) <- zip [0..] n, y > 0 && odd x]

g :: [Int] -> Int 
g n = product [ x * 3 | x <- n, x > 0 && odd x ]

--Rec 
q :: [Int] -> Int 
q [] = 1 
q (x:xs) 
		| x > 0 && odd x = (x * 3) * q xs 
		| otherwise = q xs

--Higher Order Functions 
r :: [Int] -> Int 
r n = foldr (*) 1 (map (*3) (filter (\x -> (x > 0 && odd x)) n))

test3:: [Int] -> Bool 
test3 xs = g xs == q xs && q xs == r xs

-- 2010 December--
--1.
f :: [Int] -> Int 
f n = product [div x 2| x <- n, even x]

m :: [Int] -> Int
m [] = 1
m (x:xs)
		|even x = (div x 2) * m xs 
		|otherwise = m xs 

h :: [Int] -> Int 
h n = foldr (*) 1 (map (\x -> (div x 2)) (filter (\x -> even x) n))

test4 :: [Int] -> Bool 
test4 xs = f xs == m xs && m xs == h xs 

-----------12/11/2014------------

-- 15. Get all the numbers form a list that are between 2 and 10 plus 13
getNumbersInAList:: [Int] -> [Int]
getNumbersInAList n = filter (\x -> x > 2 && x < 10 || x == 13) n

-- 16. Get all the lower letters form a string that are in the first half of the alphabet
getLowerLettersComp:: String -> String 
getLowerLettersComp l = [ x | x <- l, isLetter x && isLower x && x > 'a' && x < 'm']

getLowerLettersRec:: String -> String 
getLowerLettersRec [] = []
getLowerLettersRec (x:xs)
						|isLetter x && isLower x && x > 'a' && x < 'm' = x : getLowerLettersRec xs
						|otherwise = getLowerLettersRec xs

getLowerLettersHOF:: String -> String 
getLowerLettersHOF l = filter (\x -> isLetter x && isLower x && x >= 'a' && x < 'm') l

test5:: String -> Bool 
test5 xs = getLowerLettersComp xs == getLowerLettersRec xs && getLowerLettersRec xs == getLowerLettersHOF xs 

-- 17. Get all the lower letters form a string that are in the second half of the alphabet
getLowerLettersFromSecondPartOfTheAlphabet:: String -> String 
getLowerLettersFromSecondPartOfTheAlphabet l = filter (\x -> isLetter x && isLower x && x >= 'm' && x <= 'z') l

-- 18. Rewrite 16 but now letter casing should not matter
-- 16. Get all letters form a string that are in the first half of the alphabet
getLettersFromFirstPartOfTheAlphabet:: String -> String 
getLettersFromFirstPartOfTheAlphabet l = filter (\x -> ((toLower x) >= 'a') && ((toLower x) >= 'm')) l

-- 19. Rewrite 17 but now letter casing should not matter
--getLettersFromSecondPartOfTheAlphabet:: String -> String -- 
--getLettersFromSecondPartOfTheAlphabet l = filter (\x -> isLetter x <-

-- 20. We are given a list of tuples with the flowing form (String, Int) - you should take from 
-- it only those whose second element is even
takeSecondElementWhichIsEven:: [(String, Int)] -> [(String, Int)]
takeSecondElementWhichIsEven xs = [(x, y) |(x, y) <- xs, even y]

-- 21. We are given a list of tuples with the flowing form (String, Char, Int) - you should take from 
-- it only thouse whose third element is even
a:: [(String, Char, Int)] -> [(String, Char, Int)] 
a xs = [(x, y, z) | (x, y, z) <- xs, even z]

--HOF
aHOF:: [(String, Char, Int)] -> [(String, Char, Int)] 
aHOF xs = filter (\(x, y, z) -> even z) xs

-- 22. We are given a list of tuples with the flowing form (String, Char, Int) - you should take from 
-- it only thouse whose third element is even and their second element is a Letter
filterBySecondAndThirdElement:: [(String, Char, Int)] -> [(String, Char, Int)]
filterBySecondAndThirdElement xs = [(x,y,z)|(x,y,z) <- xs, isLetter y && even z]

--HOF
filterBySecondAndThirdElementHOF:: [(String, Char, Int)] -> [(String, Char, Int)]
filterBySecondAndThirdElementHOF xs = filter (\(x,y,z) -> ((isLetter y) && (even z))) xs

test6:: [(String, Char, Int)] -> Bool 
test6 xs = filterBySecondAndThirdElement xs == filterBySecondAndThirdElementHOF xs 

-- 23. We are given a list of Strings, take only those that have an even number of letters
takeStringsWithEvenNumberOfLettres:: [String] -> [String]
takeStringsWithEvenNumberOfLettres xs = [x | x <- xs, even (length x)] ---

takeStringsWithEvenNumberOfLettresHOF:: [String] -> [String]
takeStringsWithEvenNumberOfLettresHOF xs = filter (\x -> (even (length x))) xs ---

test15:: [String] -> Bool 
test15 xs = takeStringsWithEvenNumberOfLettres xs == takeStringsWithEvenNumberOfLettresHOF xs

-- 24. We are given a list of Strings - take only those Strings that consist of capital letters
-- Note: It is a good idea to use a helper function here
takeStringsConsistingOfCapitalLettres:: [String] -> [String]
takeStringsConsistingOfCapitalLettres xs = [x | x <- xs, isConsistingOnlyOfCapitalLetters x]

isConsistingOnlyOfCapitalLetters:: String -> Bool
isConsistingOnlyOfCapitalLetters xs = and [isUpper x | x <- xs]

--HOF
takeStringsConsistingOfCapitalLettresHOF:: [String] -> [String]
takeStringsConsistingOfCapitalLettresHOF xs = filter (isConsistingOnlyOfCapitalLetters) xs 

test14:: [String] -> Bool 
test14 xs = takeStringsConsistingOfCapitalLettres xs == takeStringsConsistingOfCapitalLettresHOF xs 

-- 25. We are given a list of Strings - take only those strings that only consist of digits
-- Note: It is a good idea to use a helper function here
-- With Helper Function --
doesTheStringContainsOnlyDigits:: String -> Bool 
doesTheStringContainsOnlyDigits xs = and [isDigit x| x <- xs] 

takeStringsConsistingOfDigits:: [String] -> [String]
takeStringsConsistingOfDigits xs = [x |x <- xs, doesTheStringContainsOnlyDigits x]

--HOF
takeStringsConsistingOfDigitsHOF:: [String] -> [String]
takeStringsConsistingOfDigitsHOF xs = filter (doesTheStringContainsOnlyDigits) xs

test13:: [String] -> Bool 
test13 xs = takeStringsConsistingOfDigits xs == takeStringsConsistingOfDigitsHOF xs

-- 26. We are given a list of bools - take only the True elements
takeTruthElementsFromAListOfBools:: [Bool] -> [Bool]
takeTruthElementsFromAListOfBools xs = [ x | x <- xs, x == True]

-- you can also try
--takeTruthElementsFromAListOfBools xs = [ x | x <- xs, x]

takeTruthElementsFromAListOfBoolsHOF:: [Bool] -> [Bool]
takeTruthElementsFromAListOfBoolsHOF xs = filter (\x -> (x == True)) xs

test12:: [Bool] -> Bool 
test12 xs = takeTruthElementsFromAListOfBools xs == takeTruthElementsFromAListOfBoolsHOF xs 

-- 27. We are given a list of bools - take only the False elements
takeFalseElementsFromAListOfBools:: [Bool] -> [Bool]
takeFalseElementsFromAListOfBools xs = [x | x <- xs, x == False]

takeFalseElementsFromAListOfBoolsHOF:: [Bool] -> [Bool]
takeFalseElementsFromAListOfBoolsHOF xs = filter (\x -> (x == False)) xs

test11:: [Bool] -> Bool 
test11 xs = takeFalseElementsFromAListOfBools xs == takeFalseElementsFromAListOfBoolsHOF xs

-- 28. We are given a list of Ints - take only those ints that are the ASCII codes of Letters
-- Here you should use the "chr" function. It takes an Int and returns the coresponding letter.
-- The "chr" function can be viewed as the opposite of the "ord" function
-- Example:
-- chr 97 == 'a'
-- chr 98 == 'b'
-- chr 99 == 'c'
-- chr 33 == '!'
checkIfASCIICodeAndReturnCorrespondingLetter:: [Int] -> [Char]
checkIfASCIICodeAndReturnCorrespondingLetter xs = [chr x | x <- xs] 
-- Тук филтър подходящо ли е да се използва изобщо?
-- Да, подхадящо е - като цяло трябва да се комбинация между chr и isLetter за филтър

-- 29. We are given a list of strings - take only those strings that start with the char 'K'
-- How can you take the first element of a list? Hint: there is a function

-- You shold check if the string is not [] because head [] - gives an error
-- Do this everywhere you use head
takeTheStringsWhichBeginWithK:: [String] -> [String]
takeTheStringsWhichBeginWithK xs = [x | x <- xs, x /= [] && head x == 'K']

takeTheStringsWhichBeginWithKHOF:: [String] -> [String]
takeTheStringsWhichBeginWithKHOF xs = filter (\x -> (head x) == 'K') xs --- Не работи както трябва---

test10:: [String] -> Bool
test10 xs = takeTheStringsWhichBeginWithK xs ==takeTheStringsWhichBeginWithKHOF xs 

-- 30. We are given a list of strings - take only those strings that do NOT start with the char 'A'
-- How can you take the first element of a list? Hint: there is a function

takeTheStringsWhichDoNotBeginWithA:: [String] -> [String]
takeTheStringsWhichDoNotBeginWithA xs = [x | x <- xs, head x /= 'A']

--HOF
takeTheStringsWhichDoNotBeginWithAHOF :: [String] -> [String]
takeTheStringsWhichDoNotBeginWithAHOF xs = filter (\x -> ((head x) /= 'A')) xs --- И тук не работи както трябва---

test9:: [String] -> Bool 
test9 xs = takeTheStringsWhichDoNotBeginWithA xs == takeTheStringsWhichDoNotBeginWithAHOF xs 

-- 31. Take all the elements for a list of ints that are not 42
takeElementsDifferentFromFourtyTwo:: [Int] -> [Int]
takeElementsDifferentFromFourtyTwo xs = [x | x <- xs, x /= 42]

--HOF
takeElementsDifferentFromFourtyTwoHOF:: [Int] -> [Int]
takeElementsDifferentFromFourtyTwoHOF xs = filter (/= 42) xs

test7:: [Int] -> Bool
test7 xs = takeElementsDifferentFromFourtyTwo xs == takeElementsDifferentFromFourtyTwoHOF xs 

-- 32. Take all the elements form a list of ints that are in the range 11 to 33 but excluding 22
takeCertainElemnts:: [Int] -> [Int]
takeCertainElemnts xs = [x | x <- xs, x > 11 && x < 33 && x /= 22]

--HOF
takeCertainElemntsHOF:: [Int] -> [Int]
takeCertainElemntsHOF xs = filter (\x -> (x > 11) && (x < 33) && (x /= 22)) xs

test8:: [Int] -> Bool 
test8 xs = takeCertainElemnts xs == takeCertainElemntsHOF xs

-- 33. Write a functions that returns "All done with filtering :)"

----------------------------------------------------------------------------------------------------
--------- 									Modifying									   ---------
----------------------------------------------------------------------------------------------------

-- 34. We are given a list of ints - increase their values with 10
increaseWithTen:: [Int] -> [Int]
increaseWithTen n = map (+10) n

-- 35. We are given a list of ints - dcrease there values with 10
decreaseWithTen:: [Int] -> [Int]
decreaseWithTen n = map (+ (- 10)) n

-- 36. Multiply all the Ints form a list by 7 and return a new list
multiplyBySeven:: [Int] -> [Int]
multiplyBySeven n = map (*7) n

-- 37. Divide all the ints form a list by 3 and return a new list
divideByThree:: [Int] -> [Int]
divideByThree n = map (`div` 3) n

-- 38. We are give a list of ints - increase there values with 10 and multiply them by 3
increseAndMultiply:: [Int] -> [Int]
increseAndMultiply n = map (\x -> ((x + 10) * 3)) n

-- 39. Convert all the letter to Upper Case in a stirng
--List Comprehension
convertToUppercase:: String -> String 
convertToUppercase l = [toUpper x | x <- l, isLetter x]

--Recursion version 
convertToUppercaseRec:: String -> String 
convertToUppercaseRec [] = []
convertToUppercaseRec (x:xs)
							| isLetter x = toUpper x : convertToUppercaseRec xs
							| otherwise = convertToUppercaseRec xs

--Higher Order Functions  
convertToUppercaseHOF:: String -> String 
convertToUppercaseHOF l = map (toUpper) (filter (isLetter) l)

-- 1 < x < 8
-- 1 == x == 8

--Test 
test :: String -> Bool 
test xs = convertToUppercase xs ==  convertToUppercaseRec xs && convertToUppercaseRec xs == convertToUppercaseHOF xs

-- 40. Convert all the letter to Lower Case in a string
convertToLowerCase:: [Char] -> String 
convertToLowerCase l = map (toLower) (filter (isLetter) l)


-- 41. We are given a String of digits - convert it to a lits of Ints that are the same as the digitis
-- Example: 
-- "13409" -> [1,3,4,0,9]
convertToInt:: String -> [Int]
convertToInt n = map (digitToInt) n

-- 42. Convert a string to a list of ASCII Codes that represent the code of every char in the string
-- Hint: you should use "ord"
convertStringToAASCIICodes :: String -> String 
convertStringToAASCIICodes l = undefined


-- 43. Convert a list of Ints to a String, by transforming every int to its ASCII code char representation
-- Hint: you should use "chr"

-- 44. We are given a list of strings - make a new string consisiting only of the first letter of every string in the 
-- first list

-- 45. We are given a list of Bool - conver them to their negation value and make a new list

-- 46. -- 44. We are given a list of strings - make a new string consisiting only of the last letter of every string in the 
-- first list

