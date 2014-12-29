-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (2/3 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, mod x 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
		| mod x 2 == 0 = (div x 2):halveTail
		| otherwise = halveTail
		where halveTail = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
		| (lo <= x && x <= hi) = x:inRangeTail
		| otherwise = inRangeTail
		where inRangeTail = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version

countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
		| (x > 0) = 1 + countPositivesRecTail
		| otherwise = countPositivesRecTail
		where countPositivesRecTail = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs 



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round ((fromIntegral x)  * 0.90)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount x | x <- xs, (discount x) <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec [x] = if (discount x) <= 19900 then (discount x) else 0
pennypincherRec (x:xs)
		| (discount x) <= 19900 = (discount x) + getValue
		| otherwise = getValue
		where getValue = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt x | x <- str, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (ch:str) 
		| isDigit ch = digitToInt ch * multDigitsTail  
		| otherwise = multDigitsTail
		where multDigitsTail = multDigitsRec str

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str == multDigitsRec str



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (h:str) = toUpper h : [toLower x | x <- str]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (h:str) = toUpper h : lowerd str
						where
							lowerd [] = [] 
							lowerd (sh:str) = toLower sh : lowerd str

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise str = capitalise str == capitaliseRec str



-- 7. title
lowerWord :: String -> String
lowerWord str = [toLower x | x <- str]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (firstWord:otherWords) = capitalise firstWord : [if length x > 3 then capitalise x else lowerWord x | x <- otherWords]

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (firstWord:otherWords) = capitaliseRec firstWord : prossesWords otherWords
	where 
		prossesWords [] = []
		prossesWords (word:restOfWords)
			| length word > 3 = capitalise word : prossesWords restOfWords
			| otherwise = lowerWord word : prossesWords restOfWords

-- mutual test
prop_title :: [String] -> Bool
prop_title str = title str == titleRec str




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len crosswords = [x | x <- crosswords, length x == len && x !! inPosition == letter]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec letter inPosition len (word:crosswords)
		| length word == len && word !! inPosition == letter = word:findCrossword
		| otherwise = findCrossword
		where findCrossword = crosswordFindRec letter inPosition len crosswords

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter inPosition len crosswords = crosswordFind letter inPosition len crosswords == crosswordFindRec letter inPosition len crosswords



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [i | (ch, i) <- zip str [0..], ch == goal]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec str goal = searchInner str goal 0
	where 
		searchInner [] _ _ = []
		searchInner (x:xs) goal i
								| x == goal = i : searchInner xs goal (i+1)
								| otherwise = searchInner xs goal (i+1)

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str goal = search str goal == searchRec str goal 


-- 10. contains

suffixes :: String -> [String]
suffixes xs = [drop i xs | i <- [0..length xs]]

-- List-comprehension version
contains :: String -> String -> Bool
contains str substr = [] /= [True | s <- suffixes str, isPrefixOf substr s] 

-- Recursive version
containsRec :: String -> String -> Bool
containsRec _ [] = True
containsRec [] substr = False
containsRec str substr = isPrefixOf substr str || containsRec (tail str) substr

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains str substr = contains str substr == contains str substr

