import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

isInInterval :: Int -> Bool
isInInterval n 
			| 0 <= n && n <= 15 = True 
			| otherwise = False

-- isInInterval n = 0 <= n && n <= 15

isInSecondInterval :: Int -> Bool
isInSecondInterval n = isInInterval n && n /= 11

isInThirdInterval :: Int -> Bool
isInThirdInterval n = isInInterval n || n == 19 

isInFourthInterval :: Int -> Bool
isInFourthInterval n = n > -11 && n /= 16 && n < 33 || n == 100

isInFifthInterval :: Int -> Bool
isInFifthInterval n = (2 < n) && (n <= 14)

isAfterM :: Char -> Bool
isAfterM l = l > 'm'

checkInterval :: Int -> Int
checkInterval n 
				| n > 2 &&  n < 15 = n * 13
				| n > 16 && n < 31 = div n 13
				| otherwise = error "N not in the given interval"

-- > (a -> a-> Bool), isDigit (Char -> Bool), not (Bool -> Bool)

-- Our functions is given a Char paramether and returns a String
-- if the Char is a capital letter, we return "I'm capital"
-- if the Char is a lower letter - "I'm lower"
-- if the Char is a space - "I'm a space"
-- the same for Digit
charCheck :: Char -> String 
charCheck l
			| isUpper l = "I'm capital"
			| isLower l = "I'm lower"
			| isSpace l = "I'm a space"
			| isDigit l = "I am not Int, My name is Digit"


-- Our functions takes a Char and returns a Bool
-- if the char is a digit, check if it is between 2 and 7 (inclusive)
-- if not throw an error
isInTheInterval :: Int -> Bool 
isInTheInterval n = 2 >= n && 7 <= n

isCharDigit :: Char -> Bool 
isCharDigit l 
			| isDigit l = (isInTheInterval (digitToInt l))
			| otherwise = error "Not in the given interval or not a digit"


iReturnBool :: Int -> Bool
iReturnBool = undefined

iReturnString :: Bool -> String
iReturnString = undefined

iReturnAChar :: String -> Char
iReturnAChar = undefined

-- 0, 1, 2... 9
-- '0', '1',
-- "0" ['0']

--iCombineFunctions :: Int -> Int
--iCombineFunctions n = digitToInt(iReturnAChar (iReturnString (iReturnBool n)))

-- 11 -> 'A'
-- 12 -> 'K'
-- 13 -> 'Q'
-- 14 - 'J'

numberToPlayingFaceCard :: Int -> Char
numberToPlayingFaceCard n
						| n == 11 = 'A'
						| n == 12 = 'K'
						| n == 13 = 'Q'
						| n == 14 = 'J'
						| otherwise = error "Not is scope"

-- check if a char represnet the playing car Ace (A) 
-- if not throw an error
isAce :: Char -> Bool
isAce l = l == 'A' 

-- prints a happy message if the input bool is True
-- and if it is false prints "Try again"
printMessage :: Bool -> String
printMessage b 
				| b == True = "Congrats"
				| b /= True = "Try again"

combineFunctions :: Int -> String
combineFunctions n = (printMessage (isAce (numberToPlayingFaceCard n)))