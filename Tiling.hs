module Tiling (Triangle, tiling) where


{--
*DO NOT* import any modules. 
*You are allowed to use anything available in Prelude and any syntax features* 

Recall the induction problem from Assignment 1 Written. Namely that we can
tile triangle T_n with a trapezium comprised of three equilateral triangles so 
that only the top triangle remains uncovered.

We can encode each row of the triangle by a list, and the triangle itself
by a list of rows.  That is, 

    type Triangle = [[Integer]]

Therefore, if we number the tiles we can represent tilings as follows:

     /\         [     [0],
    /\/\            [1,1,1],
   /\/\/\         [2,3,3,3,5],
  /\/\/\/\      [2,2,4,4,4,5,5] ]
     T_2            T_2 tiling

where 0 designates the position of the uncovered triangle.

Write a function 

    tiling :: Integer -> Triangle

which, given an integer n, returns a tiling for T_n encoded like the above.

========
EXAMPLES
========

NOTE your solution does not have to look identical to the following examples.
We will be conducting PROPERTY TESTING of you code.  That is, we will confirm
your board satisfies the tiling rather than comparing them with tiled boards.
For example, the tile numbers do not have to be 1, 2, 3, ... -- just unique.

> tiling 0
[[0]]

> tiling 1
[[0], [1,1,1]]

> tiling 2
[[0], [1,1,1], [2,3,3,3,5], [2,2,4,4,4,5,5]]
--}
type Triangle = [[Integer]]

rev :: Triangle -> Triangle
rev ts = reverse (map reverse ts)

firstTwoFromEach :: [[Integer]] -> [Integer]
firstTwoFromEach = concatMap (take 2)

rotate :: Triangle -> Triangle
rotate [[]] = undefined
rotate lists = take (length lists) t
    where
        t = firstTwoFromEach lists : rotate (map (drop 2) lists)

-- Merges the three traingles in order
merge :: Triangle -> Triangle -> Triangle -> Triangle
merge [] [] [] = []
merge (a:as) (b:bs) (c:cs) = (a ++ b ++ c) : merge as bs cs

incrementN' :: Integer -> [Integer] -> [Integer]
incrementN' n [] = []
incrementN' n (x:xs) | x == 0 = x : incrementN' n xs
                    | otherwise = x + n : incrementN' n xs

incrementN :: Integer -> Triangle -> Triangle
incrementN = map . incrementN'

findCount :: Triangle -> Integer
findCount [[0]] = 0
findCount lst = 4 * prevCount + 1
    where
        prevCount = findCount $ take (length lst `div` 2) lst

replaceTip :: Integer -> Triangle -> Triangle
replaceTip n (x:xs) = [n] : xs

replaceZero' :: Integer -> Integer -> Integer
replaceZero' n a = if a == 0 then n else a

replaceZero :: Integer -> Triangle -> Triangle
replaceZero _ [] = []
replaceZero n (x:xs) | x == [0] = [0] : replaceZero n xs
                     | otherwise = map (replaceZero' n) x : replaceZero n xs

tiling :: Integer -> Triangle
tiling 0 = [[0]]
tiling 1 = [[0], [1,1,1]]
tiling n = replaceZero (x*4 + 1) $ t ++ merge (incrementN x (reverse $ rotate $ rev t)) (incrementN (x*2) (reverse t)) (incrementN (x*3) (reverse $ rotate t))
    where
        t = tiling (n - 1)
        x = findCount t