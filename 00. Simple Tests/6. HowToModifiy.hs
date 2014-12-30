import Data.Char
import Data.List
import Test.QuickCheck

increaseBy13 :: [Int] -> [Int]
increaseBy13 inputList = [x + 13 | x <- inputList]

-- 1.2 С recursion

increaseBy13Rec :: [Int] -> [Int]
increaseBy13Rec [] = []
increaseBy13Rec (x:xs) = (x + 13) : increaseBy13Rec xs

increaseBy13Hof :: [Int] -> [Int]
increaseBy13Hof inputList = map (+13) inputList

-- list comprehension
transforToLower :: String -> String
transforToLower inputList = [ toLower x | x <- inputList]

-- 1.2 recursion

transforToLowerRec :: String -> String
transforToLowerRec [] = []
transforToLowerRec (x:xs) = (toLower x) : transforToLowerRec xs

-- 1.3 Higher Order Functions 
transforToLowerHof :: String -> String
transforToLowerHof inputList = map (toLower) inputList