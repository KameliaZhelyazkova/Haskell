import Data.Char
import Data.List
import Test.QuickCheck

--comprehension 
g :: [Int] -> Int
g numbers = product [(x `div` 6)| x <- numbers, x `mod` 5 == 0]

-- Recursion
f :: [Int] -> Int
f [] = 1
f (x:xs) 
		| x `mod` 5 == 0 = (x `div` 6) * f xs
		| otherwise = f xs

-- Higher order functions
f :: [Int] -> Int
--f numbers = map (product (numbers `div` 6) 

h :: [Int] -> Int
h inputList = foldr (*) 1 (map (`div` 5) (filter (\x -> x `mod` 5 == 0) inputList))