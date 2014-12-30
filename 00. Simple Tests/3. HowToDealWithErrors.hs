import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- 1.1. A function that adds 9 to all the elements of a list
sampleFunction :: [Int] -> [Int]
sampleFunction xs = [x + 9| x <- xs]

-- 1.2. A recursion variant
sampleFunctionRec :: [Int] -> [Int]
sampleFunctionRec [] = []
sampleFunctionRec (x:xs) = (x + 9) : sampleFunctionRec xs

-- 2. A function that returns a string message depending on the given char input
secondSampleFunction :: Char -> String
secondSampleFunction c 
			| c == 'a' = "I'm A"
			| c == 'c' = "I'm C"
			| otherwise = "Who am I?"

-- 3. A function that returns a bool depending if the char given is larger that 3 or 0
thirdSampleFunction :: Char -> Bool
thirdSampleFunction c = isDigit c && (digitToInt c) > 3 || (digitToInt c) == 0 
			