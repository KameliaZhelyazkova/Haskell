import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

sampleFunction :: Int -> String
sampleFunction 1 = "Hey"
sampleFunction 2 = "Hug"
sampleFunction _ = "Not 1 or 2"


secondSampleFunction :: Char -> Int
secondSampleFunction c
					| isDigit c = digitToInt c