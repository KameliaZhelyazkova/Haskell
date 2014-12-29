-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate rotationNumber input
				|rotationNumber < 0 || rotationNumber > (length input) = error "Invalid rotation number"
				|otherwise = (drop rotationNumber input) ++ (take rotationNumber input)

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
getOffsetLetter :: Int -> Char
getOffsetLetter offset = head (rotate offset "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

makeKey :: Int -> [(Char, Char)]
makeKey offset = zip (rotate 0 "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (rotate offset "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

-- TODO: find a way to store the generated list not to regenerate it
-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp charToLookFor chipherSource = 
	if length [(snd x) | x <- chipherSource, (fst x) == charToLookFor] > 0
	then head [(snd x) | x <- chipherSource, (fst x) == charToLookFor]
	else charToLookFor

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec charToLookFor [] = charToLookFor 
lookUpRec charToLookFor (firstPair:chipherSourceTail)
							| (fst firstPair) == charToLookFor = snd firstPair
							| otherwise = lookUpRec charToLookFor chipherSourceTail

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp charToLookFor chipherSource = (lookUp charToLookFor chipherSource) == (lookUpRec charToLookFor chipherSource)

-- 5.
encipher :: Int -> Char -> Char
encipher offset character = lookUp character (makeKey offset)

-- 6.
normalize :: String -> String
normalize text = [toUpper x | x <- text, (isDigit x) || (isLetter x)]

-- 7.
encipherStr :: Int -> String -> String
encipherStr offset text = [encipher offset x | x <- (normalize text)]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey originalKeys = [(snd x, fst x) | x <- originalKeys]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (headKey:tailKeys) = (snd headKey, fst headKey):(reverseKeyRec tailKeys)

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey keys = reverseKey keys == reverseKeyRec keys
-- 9.
decipher :: Int -> Char -> Char
decipher offset character = lookUp character (reverseKey (makeKey offset))

decipherStr :: Int -> String -> String
decipherStr offset text = [decipher offset x | x <- text]

-- 10.
contains :: String -> String -> Bool
contains searchTerm sourceText = isInfixOf searchTerm sourceText

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], candidate (decipherStr i str)]
	where candidate str = str `contains` "AND" || str `contains` "THE"


-- Optional Material

-- 12.
fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

splitEachFive :: String -> [String]
splitEachFive xs
			| length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
			| otherwise = [fillToFive xs]

-- 13.
prop_transpose :: String -> Bool
prop_transpose xs = ys == transpose (transpose ys)
	where ys = splitEachFive xs

-- 14.
encrypt :: Int -> String -> String
encrypt n str = concat (transpose (splitEachFive (encipherStr n str)))

-- 15.
splitEach :: Int -> String -> [String]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)

splitFiveWays :: String -> [String]
splitFiveWays xs 
			| n `mod` 5 == 0 = splitEach (n `div` 5) xs
			| otherwise = error "not a multiple of 5"
		where n = length xs

decrypt :: Int -> String -> String
decrypt n str = concat (transpose (splitFiveWays (decipherStr n str)))

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined