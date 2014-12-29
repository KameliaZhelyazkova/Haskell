-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 13/14 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go x) = [Go x]
split (Turn x) = [Turn x]
split Sit = []
split (x :#: y) = split x ++ split y


-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (x:[]) = x
join (x:xs) = x :#: join xs

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent x y = split x == split y

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

prop_split :: Command -> Bool
prop_split x = and [ f y | y <- (split x)]
		where
		f Sit = False
		f (x :#: y) = False
		f x = True



-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n c = join [ c | count <- [0..n], count < n ]

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn (360 / fromIntegral n))

-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral side n step angle = if side > 0 then (Go side :#: Turn angle) :#: (spiral (side + step) (n-1) step angle)
							else error "Negative distance"


-- Exercise 4
-- optimise
optimised :: Command -> Bool
optimised (Go x :#: Go y) = False
optimised (Turn x :#: Turn y) = False
optimised (Turn 0) = False
optimised (Go 0) = False
optimised Sit = False
optimised (x :#: y) = optimised x && optimised y
optimised x = True

optimise :: Command -> Command
optimise p | optimised p = p
		   | otherwise = optimise (f (join [ x | x <- split p, x /= Sit]))
					where
							f (Go x :#: Go y) = Go (x+y)
							f (Turn x :#: Turn y) = Turn (x+y)
							f (x :#: y) = f(x) :#: f(y)
							f (Go 0) = Sit
							f (Turn 0) = Sit
							f (Go x) = Go x
							f (Turn x) = Turn x


-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
		where
		f 0 = GrabPen black :#: Go 10
		f x = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
		g 0 = GrabPen blue :#: Go 10
		g x = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
		n = Turn 60
		p = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x :#: p :#: p
		where
		f 0 = GrabPen blue :#: Go 10
		f x = f (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: n :#: f (x-1)
		n = Turn (-60)
		p = Turn (60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
		where
		l 0 = GrabPen blue :#: Sit
		l x = n :#: r (x-1) :#: f (x-1) :#: p :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: p :#: f (x-1) :#: r(x-1) :#: n
		r 0 = GrabPen red :#: Sit
		r x = p :#: l (x-1) :#: f (x-1) :#: n :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: n :#: f (x-1) :#: l (x-1) :#: p
		f 0 = GrabPen black :#: Go 10 :#: n :#: p
		f x = f (x-1)
		n = Turn (-90)
		p = Turn (90)
		
-- Optional Exercises
peano_gosper :: Int -> Command
peano_gosper x = f x
		where
		f 0 = GrabPen blue :#: Go 10
		f x = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
		g 0 = GrabPen black :#: Go 10
		g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
		n = Turn (60)
		p = Turn (-60)
		
cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x :#: n
		where
		f 0 = GrabPen black :#: Go 10
		f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
		n = Turn (90)
		p = Turn (-90)
		
branch :: Int -> Command
branch x = g x
		where
		g 0 = GrabPen green :#: Go 10
		g x = f (x-1) :#: n :#: Branch ( Branch (g (x-1)) :#: p :#: g (x-1)) :#: p :#: f (x-1) :#: Branch ( p :#: f (x-1) :#: g (x-1)) :#: n :#: g (x-1)
		f 0 = GrabPen black :#: Go 10
		f x = f (x-1) :#: f (x-1)
		p = Turn (-22.5)
		n = Turn (22.5)
		
seg32 :: Int -> Command
seg32 x = f x :#: p :#: f x :#: p :#: f x :#: p :#: f x 
		where
		f 0 = GrabPen blue :#: Go 10
		f x = n :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: p
		p = Turn (-90)
		n = Turn 90