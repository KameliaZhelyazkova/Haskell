import Data.Char
import Data.List
import Test.QuickCheck

checkIfInRange1to14 :: Int -> Bool
checkIfInRange1to14 number = number > 1 && number < 14

filterListForValuesBetween1and14 :: [Int] -> [Int]
filterListForValuesBetween1and14 inputList = [x | x <- inputList, x > 1 && x < 14]

filterListForValuesBetween1and14Helper :: [Int] -> [Int]
filterListForValuesBetween1and14Helper inputList = [x | x <- inputList, checkIfInRange1to14 x]

filterListForValuesBetween1and14Rec :: [Int] -> [Int]
filterListForValuesBetween1and14Rec [] = []
filterListForValuesBetween1and14Rec (x:xs)
								| x > 1 && x < 14 = x : filterListForValuesBetween1and14Rec xs
								| otherwise = filterListForValuesBetween1and14Rec xs

filterListForValuesBetween1and14RecHelper :: [Int] -> [Int]
filterListForValuesBetween1and14RecHelper [] = []
filterListForValuesBetween1and14RecHelper (x:xs)
								| checkIfInRange1to14 x = x : filterListForValuesBetween1and14Rec xs
								| otherwise = filterListForValuesBetween1and14Rec xs

filterListForValuesBetween1and14Hof :: [Int] -> [Int]
filterListForValuesBetween1and14Hof inputList = filter (<1) (filter (<14) inputList)

filterOnlyNumbersThatAreDivisibaleBy7 :: [Int] -> [Int]
filterOnlyNumbersThatAreDivisibaleBy7 inputList = filter (\x -> x `mod` 7 == 0) inputList

filterListForValuesBetween1and14HofHelper :: [Int] -> [Int]
filterListForValuesBetween1and14HofHelper inputList = filter checkIfInRange1to14 inputList

checkIfInRange :: Char -> Bool
checkIfInRange letter = letter > 21'a' && letter < 'm'

filterListForValues :: String -> String
filterListForValues inputList = [x | x <- inputList, x > 'a' && x < 'm']

filterListForValuesHelper :: String -> String
filterListForValuesHelper inputList = [x | x <- inputList, checkIfInRange x]

filterListForValuesRec :: String -> String
filterListForValuesRec [] = []
filterListForValuesRec (x:xs)
								| x > 'a' && x < 'm' = x : filterListForValuesRec xs
								| otherwise = filterListForValuesRec xs

filterListForValuesRecHelper :: String -> String
filterListForValuesRecHelper (x:xs)
								| checkIfInRange x = x : filterListForValuesRecHelper xs
								| otherwise = filterListForValuesRecHelper xs

filterListForValuesHof :: String -> String
filterListForValuesHof inputList = filter (>'a') (filter (<'m') inputList)

filterListForValuesHofHelper :: String -> String
filterListForValuesHofHelper inputList = filter checkIfInRange inputList