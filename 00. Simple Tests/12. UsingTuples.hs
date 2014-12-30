-- The most common function of tuples is to group values
-- for example a tuple of 3 ints can be a point in the 3D space (1,0,0) or a tuple of 2 
-- ints can be a point the 2D space - (0,1)
-- a tuple of 3 floats can be a vetor (1.1, 1.1, 1.1)
-- Or a tuple of 3 strings can contain the 3 animal names - ("Doggy", "Bunny", "Bear")
-- ('a', '2')
-- (1, "Hello")
-- ('c', "World")

-- 1. Spesific function for tuples
-- the most common functuns used for tuples are:
-- fst - gets the first element of a tuple
-- Example: fst (123, 456) == 123
getFirstElement :: (Int, Int) -> Int
getFirstElement n = fst n

-- snd - gets the second element of a tuple
-- Example: snd (971, 3455) = 3455
getSecondElement :: (Int, Int) -> Int
getSecondElement n = snd n

-- 2. Using pattern matching
-- we can use pattern matching to get a spesifice element of a tuple
-- Get the 4th element of a tuple
getFourthElement :: (Int, Int, Int, Int) -> Int
getFourthElement (_,_,_,fourth) = fourth

-- get the 3rd element of a tuple of 5 ints
getThirdElement :: (Int, Int, Int, Int, Int) -> Int
getThirdElement (_,_,third,_,_) = third

-- 3. Using list of tuples. We can use all the technices for working with list (list comprehension, recursion, HOF) when dealing with
-- list of tuples
-- We are given a list of tuples consisting of 2 ints. Compouse a new list of ints that compouse only from 
-- the first element
-- Example: [(1,3),(33,1),(44,123)]
transformList :: [(Int, Int)] -> [Int]
transformList xs = [fst x | x <- xs]

takeSecondElementFromEachTuple:: [(Int, Int)] -> [Int]
takeSecondElementFromEachTuple xs = [snd x | x <- xs]

-- We can also use patern matching when getting elements from the list
transformList2 :: [(Int, Int)] -> [Int]
transformList2 xs = [x | (x, y) <- xs] 

-- 4. Pattern matching 2 tuples at the same time
-- Substituete 2 vectors
substituteVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
substituteVectors (a, b) (c, d) = ((a - c), (b - d))

-- sum 2 vectors
sumVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumVectors (a, b) (c, d) = ((a + c), (b + d))

-- Find the dot product of 2 vectors
dotProductOfVectors :: (Int, Int) -> (Int, Int) -> Int
dotProductOfVectors (a, b) (c, d) = ((a*c) + (b*d))