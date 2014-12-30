-- 1.1 Lsit Comprehension
getHalfsOfEevens :: [Int] -> [Int]
getHalfsOfEevens inputList = [x `div` 3 | x <- inputList, even x]

-- 1.2 Recursion
getHalfsOfEevensRec :: [Int] -> [Int]
getHalfsOfEevensRec [] = []
getHalfsOfEevensRec (x:xs)
						| even x = x : getHalfsOfEevens xs
						| otherwise = getHalfsOfEevens xs

-- 1.3. Higher Order Functions
getHalfsOfEevensHof :: [Int] -> [Int]
getHalfsOfEevensHof inputList = map (`div` 3) (filter (even) inputList)

h :: [Int] -> Int
h inputList = foldr (*) 1 (map (`div` 5) (filter (\x -> x `mod` 5 == 0) inputList))

-- [1, 2, 3] => 1 * 1 * 2 * 3 = 6
-- --> = XOR
