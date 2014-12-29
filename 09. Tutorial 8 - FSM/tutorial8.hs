-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 11 - due: 28/29 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (x,_,_,_,_) = x
alph   (_,x,_,_,_) = x
start  (_,_,x,_,_) = x
final  (_,_,_,x,_) = x
trans  (_,_,_,_,x) = x


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm s a = [ z | (x,y,z) <- (trans fsm), x == s, a == y ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom m q [] = q `elem` final m
acceptsFrom m q (x:xs) = last' [acceptsFrom m y xs | y <- delta m q x]

last' :: [Bool] -> Bool
last' [] = False
last' x = last x

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical q = nub (sort q)


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm xs ch = canonical (concat [ delta fsm x ch | x <- xs ])


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm xs = canonical (xs ++ [ ddelta fsm x a | a <- alph fsm, x <- xs ])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm xs | next fsm xs == xs = xs
				 | otherwise = reachable fsm (next fsm xs)


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm xs = [ x | x <- xs, isFinal x (final fsm)]

isFinal :: (Ord q) => [q] -> [q] -> Bool
isFinal y finals = or [ x `elem` y | x <- finals]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm xs = [ (x,ch,(ddelta fsm x ch)) | x <- xs,  ch <- alph fsm]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = (reachable fsm [[start fsm]],
					 alph fsm,
					 [start fsm],
					 dfinal fsm (reachable fsm [[start fsm]]),
					 dtrans fsm (reachable fsm [[start fsm]]))


