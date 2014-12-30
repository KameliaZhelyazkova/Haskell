-- Our first function takes 2 Int parameters, and we name them - a and b
-- In our body we call secondFunction with a and b
firstFunction :: Int -> Int -> Int
firstFunction a b = secondFunction a b

-- Our second function takes the same type of parameters - 2 ints
-- After the call of secondFunction a and b for firstFunction become a1 and b2
-- In the body of this function we call thirdFunction with a1 and b1
secondFunction :: Int -> Int -> Int
secondFunction a1 b1 = thirdFunction a1 b1

-- Similarly to secondFunction, when we call thirdFunction with a1 and b1 they become a2 and b2
thirdFunction :: Int -> Int -> Int
thirdFunction a2 b2 = a2 * b2

-- can you do a similar thing but at the end of the chaing the numbers will be divided? 
firstFunction1:: Int -> Int -> Int
firstFunction1 a b = secondFunction1 a b 

secondFunction1:: Int -> Int -> Int
secondFunction1 a1 b1 = thirdFunction1 a1 b1

thirdFunction1:: Int -> Int -> Int
thirdFunction1 a2 b2 = div a2 b2 
-- so at the end a == a1 == a2
-- and b == b1 == b2

firstFunction2:: Int -> Int -> Int
firstFunction2 a b = secondFunction1 a (b*3) 

secondFunction2:: Int -> Int -> Int
secondFunction2 a1 b1 = thirdFunction1 a1 (b1-10)

thirdFunction2:: Int -> Int -> Int
thirdFunction2 a2 b2 = div a2 b2 

-- This is a standard function that takes 2 numbers and sums them 
sumTwoNumbers:: Int -> Int -> Int
sumTwoNumbers a b = a + b



-- here we call our sumTwoNumbers function with a, so
-- the "a" parameter from executeOperation becomes the "a" parameter from the sumTwoNumbers function
-- and the square of "a" becomes the "b" parameter
executeOperation:: Int -> Int
executeOperation a = (sumTwoNumbers a a*a)

