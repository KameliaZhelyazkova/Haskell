-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 16/17 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a.
uppers :: String -> String
uppers input = map (toUpper) input

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (*2) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (toPound) xs
					where toPound price = (fromIntegral price) / 100 

-- d.
uppers' :: String -> String
uppers' input = [toUpper x | x <- input]

prop_uppers :: String -> Bool
prop_uppers input = uppers input == uppers' input



-- 2. Filter
-- a.
alphas :: String -> String
alphas input = filter (isAlpha) input

-- b.
rmChar ::  Char -> String -> String
rmChar forbidenChar input = filter (/=forbidenChar) input

-- c.
above :: Int -> [Int] -> [Int]
above numberBorder xs = filter (>numberBorder) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals pairs = filter (\x -> fst x /= snd x) pairs 

-- e.
rmCharComp :: Char -> String -> String
rmCharComp forbidenChar xs = [x | x <- xs, x /= forbidenChar]

prop_rmChar :: Char -> String -> Bool
prop_rmChar forbidenChar xs = rmChar forbidenChar xs == rmCharComp forbidenChar xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = undefined

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map (reverse) (filter (\x -> even (length x)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec [] = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold [] = 1
productFold list = foldr (*) 1 list

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold [] = True
andFold list = foldr (&&) True list 

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold [] = []
concatFold list = foldr (++) [] list 

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] text = text
rmCharsRec (x:xs) text = rmCharsRec xs (rmCharComp x text)

rmCharsFold :: String -> String -> String
rmCharsFold [] text = text
rmCharsFold forbidenString text = foldr (\x -> rmCharComp x) text forbidenString

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = all (== (head xs)) xs 

-- b.
valid :: Matrix -> Bool
valid = undefined

-- 6.

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- Optional material
-- 9.