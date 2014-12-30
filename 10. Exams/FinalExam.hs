import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- 1

-- 1 a
f:: [Int] -> Bool
f [] = error "List is empty"
f (x:xs) = and [x*2 <= y | (x, y) <- zip (x:xs) xs]

fTest = f [1,2,7,18,47,180] == True && f [17] == True && f [1,3,5,16,42] == False && f[1,2,6,6,13] == False

-- 1 b
g:: [Int] -> Bool
g [] = error "Empty list"
g [a] = True
g (x:y:xs) = (x*2 <= y) && (g (y:xs))

gTest = g [1,2,7,18,47,180] == True && g [17] == True && g [1,3,5,16,42] == False && g [1,2,6,6,13] == False

-- Test
fp_prop xs = not (null xs) ==> g xs == f xs

global1 = fTest && gTest

-----------------------------
-- 2

-- 2 a
p:: [Int] -> Int
p list = sum [(x*x*x)| x <- list, x > 0]

pTest = p [-17] == 0 && p [] == 0 && p [-3, 3, 1, -3, 2, -1] == 36 && p [2,6,-3,0,3,-7,2] == 259 && p [4,-2,-1,-3] == 64

-- 2 b
q:: [Int] -> Int
q [] = 0
q (x:xs)
	| x > 0 = (x*x*x) + q (xs)
	| otherwise = q (xs)

qTest = q [-17] == 0 && q [] == 0 && q [-3, 3, 1, -3, 2, -1] == 36 && q [2,6,-3,0,3,-7,2] == 259 && q [4,-2,-1,-3] == 64

qp_prop xs = q xs == p xs


-- 2 c
r:: [Int] -> Int
r list = foldr (+) 0 (map (^3) (filter (>0) list))

rTest = r [-17] == 0 && r [] == 0 && r [-3, 3, 1, -3, 2, -1] == 36 && r [2,6,-3,0,3,-7,2] == 259 && r [4,-2,-1,-3] == 64

qpr_prop xs = q xs == p xs && p xs == r xs

global2 = pTest && qTest && rTest

---------------------------------

--- template
data Expr = X
			| Const Integer
			| Expr :+: Expr
			| Expr :-: Expr
			| Expr :*: Expr
			| IfLt Expr Expr Expr Expr
			deriving (Eq, Ord)

showExpr :: Expr -> String
showExpr X = "X"
showExpr (Const n) = "Const" ++ show n
showExpr (p :+: q) = "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q) = "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"
showExpr (p :-: q) = "(" ++ showExpr p ++ "-" ++ showExpr q ++ ")"
showExpr (IfLt p q r t) = "IfLt" ++ showExpr p ++ showExpr q ++ showExpr r ++ showExpr t

instance Show Expr where
	show = showExpr

-- 3 a

eval :: Expr -> Integer -> Integer
eval (X) x = x
eval (Const x) _ = x
eval (p :+: q) x = (eval p x) + (eval q x)
eval (p :*: q) x = (eval p x) * (eval q x)
eval (p :-: q) x = (eval p x) - (eval q x)
eval (IfLt p q r t) x
			| (eval p x) < (eval q x) = (eval r x)
			| otherwise = (eval t x)

-- Task 3 a test
evalTest = eval (X :+: (X :*: Const 2)) 3 == 9 &&
			eval (X :-: (X :*: Const 3)) 0 == 0 &&
			eval (X :-: (X :*: Const 3)) 7 == (-14) &&
			eval (X :*: X) 2 == 4 &&
			eval (Const 15 :+: (Const 7 :*: (X :-: Const 1))) 0 == 8 &&
			eval (X :-: (X :+: X)) 4 == -4

-- 3 b

protect :: Expr -> Expr
protect X = X
protect (Const x) = if x < 0 then Const 0 else Const x
protect (p :+: q) = (protect p) :+: (protect q)
protect (p :*: q) = (protect p) :*: (protect q)
protect (p :-: q) = IfLt (protect p) (protect q) (Const 0) ((protect p) :-: (protect q))

-- Task 3 b test
protectTest = protect (X :+: (X :*: Const 2)) == (X :+: (X :*: Const 2)) &&
			   protect (X :-: (X :*: Const 3)) == IfLt X (X :*: Const 3) (Const 0) (X :-: (X :*: Const 3)) &&
			   protect (X :+: X) == (X :+: X) &&
			   protect (Const 15 :+: (Const 7 :*: (X :-: Const 1))) == Const 15 :+: (Const 7 :*: IfLt X (Const 1) (Const 0) (X :-: Const 1)) &&
			   protect (X :-: (X :+: X)) == IfLt X (X :+: X) (Const 0) (X :-: (X :+: X))


-- extended eval test

extendedEvalTest = eval (protect (X :+: (X :*: Const 2))) 3 == 9 &&
					eval (protect (X :-: (X :*: Const 3))) 0 == 0 &&
					eval (protect (X :-: (X :*: Const 3))) 7 == 0 &&
					eval (protect (X :+: X)) 2 == 4 &&
					eval (protect (Const 15 :+: (Const 7 :*: (X :-: Const 1)))) 0 == 15 &&
					eval (protect (X :-: (X :+: X))) 4 == 0

-- QucikTest:
prop3:: Expr -> Integer -> Bool
prop3 ex n = eval ex n == eval(protect ex) n

-- Task 3 test
global3 = evalTest && protectTest && extendedEvalTest


-- Exam test
global = global1 && global2 && global3