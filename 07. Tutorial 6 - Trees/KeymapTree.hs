-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

							   
-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ l r) = max (1 + depth r) (1 + depth l)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ [(k,a)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f 
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
	where
		f Leaf = Nothing
		f (Node k a left right) 
                | key == k  = Just a
								| key <= k  = f left
								| otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList x = foldr (uncurry (set)) Leaf x

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT x map = fromList [ m | m <- toList map, m < x ]

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT x map = fromList [ m | m <- toList map, m > x ] 

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge m1 m2 = fromList $ toList m1 ++ toList m2 

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del x map = fromList [ m | m <- toList map, x /= m]

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select test map = fromList [ m | m <- toList map, test (snd m) ] 