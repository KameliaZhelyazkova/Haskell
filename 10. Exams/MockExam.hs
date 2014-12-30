import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- 1. При рочитането на задачата разбираме, че ще ни трябват няколко фукции с които да определяме дали определен символ 
-- е карта за игра и дали определен сивмол е от картите А, К, Q, J

-- При тази фукция единствено проверяваме дали символа е един от следните А, К, Q, J
isFaceCard :: Char -> Bool
isFaceCard s = s == 'A' || s == 'K' || s == 'Q' || s == 'J'

-- Тук проверяваме дали символа е въобще карта за игра. За това да е вярно, имаме няколко възможности:
-- символа е число между 2 и 9 + 0
-- симовла е една от картите А, К, Q, J (вече имаме тази функция, така че можем да я преизползваме)
-- във всички други случаи, символа не е карта
---- Имаме и where binding, който ни спеставяа превръщането на символа в число
isCardChar :: Char -> Bool
isCardChar s
		| isDigit s = ((digitValue >= 2) 
			&& (digitValue <= 9)) || digitValue == 0
		| isFaceCard s = True
		| otherwise = False
		where digitValue = digitToInt s

-- След като вече имаме помощните функции, работа ни се олеснява, значиетолно
-- При List Comprehensopn-a филтрираме целия си лист според това дали са символи на карти
-- и след това проверяваме дали те са символи на картите от А, К, Q, J. По условие, всички символи в даден стринг 
-- трябва да са карти от тази група и заради това използваме and функцията, кяото ни агрегира лист от bool-ове 
-- до единична стойност
f :: String -> Bool
f input = and [isFaceCard x | x <- input, isCardChar x]

g :: String -> Bool
g [] = True
g (x:xs)
	| isCardChar x = isFaceCard x && g xs
	| otherwise = g xs

prop_fg :: String -> Bool
prop_fg list = f list == g list

h :: String -> Bool
h input = foldr (&&) True (map (isFaceCard) (filter (isCardChar) input))

prop_fgh :: String -> Bool
prop_fgh list = (f list == g list) && (g list == h list) 

t :: [a] -> [a]
t input = concat [if (even i) then [x] else [x,x] | (x,i) <- (zip input [0..])]

u :: [a] -> [a]
u [] = []
u [z] = [z, z]
u (x:y:xs) = x : y : y : (u xs)

prop_tu :: (Eq a) => [a] -> Bool
prop_tu input = t input == u input

data Proposition = Var String
				| F
				| T
				| Not Proposition
				| Proposition :|: Proposition
				| Proposition :&: Proposition
				deriving (Eq, Ord, Show)


isNorm :: Proposition -> Bool
isNorm (Var _) = True
isNorm T = True
isNorm F = True
isNorm (Not (x:|:y)) = False
isNorm (Not (x:&:y)) = False
isNorm (Not (Not _)) = False
isNorm (Not _) = True
isNorm (p:|:v) = isNorm p && isNorm v
isNorm (p:&:v) = isNorm p && isNorm v

norm :: Proposition -> Proposition
norm (Var x) = Var x
norm (Not F) = T
norm (Not T) = F
norm (Not (Var a)) = Not (Var a)
norm (Not (p:|:q)) = norm (Not p) :|: norm (Not q)
norm (Not (p:&:q)) = norm (Not p) :&: norm (Not q)
norm (Not (Not a)) = norm a
norm (p:|:q) = norm p :|: norm q
norm (p:&:q) = norm p :&: norm q