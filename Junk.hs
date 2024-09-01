
-- Junk

{-condIncr' :: Int -> Bool -> Int
condIncr' a True = a + 1
condIncr' a False = a-}

{-trim' :: a -> b -> a
trim' a _ = a-}

{-enumerate' :: a -> Int -> (Int, a)
enumerate' a n = (n, a)-}

{-trim (a:as) (b:bs) | null bs = [a]
                   | null as = [a]
                   | otherwise = a : trim as bs-}

{-condIncr (a:as) (b:bs) | null as = if b then [a + 1] else [a]
                       | null bs = if b then [a + 1] else [a]
                       | b = a + 1 : condIncr as bs
                       | otherwise = a : condIncr as bs-}


  {-enumerate (a:as) = enumerate' (a:as) 0
  where
    enumerate' [] _ = []
    enumerate' (a:as) n = (n, a) : enumerate' as (n + 1)-}

    
    

{- firstCommon :: Eq a => [a] -> a
firstCommon [] = undefined
firstCommon [a] = a
firstCommon (a:as) = findMaxx (firstCommon' (a:as))

firstCommon' :: Eq a => [a] -> [(Integer, a)]
firstCommon' [] = undefined
firstCommon' [a] = [(1, a)]
firstCommon' (a:as) = findCommon (findElements (a:as)) (a:as)

findElements :: Eq a => [a] -> [a]
findElements [] = []
findElements (a:as) | checkInList a as = findElements as
                    | otherwise = a : findElements as

checkInList :: Eq a => a -> [a] -> Bool
checkInList _ [] = False
checkInList a (b:bs) | a == b = True
                     | otherwise = checkInList a bs

countElement :: Eq a => a -> [a] -> Integer
countElement _ [] = 0
countElement a (b:bs) | a == b = 1 + countElement a bs
                      | otherwise = countElement a bs

findCommon :: Eq a => [a] -> [a] -> [(Integer, a)]
findCommon [] _ = []
findCommon _ [] = []
findCommon (a:as) (b:bs) = (countElement a (b:bs), a) : findCommon as (b:bs)
-}

{-countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays (a:as) (b:bs) | checkInList b as && a /= b = 1 + countWays as bs
                        | not (checkInList b as) && a /= b = 0
                        | otherwise = countWays as bs

countElement :: (Eq a) => a -> [a] -> Integer
countElement _ [] = 0
countElement a (b : bs) | a == b = 1 + countElement a bs
                        | otherwise = countElement a bs-}

{-countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays (a:as) (b:bs) | not (checkInList a (b:bs)) = 0
                        | a == b = countWays as bs
                        | a /= b = countWays as bs - 1-}

{- countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays a [b] = countElement b a
countWays (a:as) (b:bs) | a == b = countWays as bs
                        | checkInList b as = countWays as bs
                        | otherwise = 0-}

{-countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays (a:as) (b:bs) | a /= b = countWays (a:as) bs + countWays as (b:bs)
                        | otherwise = countWays as bs

checkInList :: Eq a => a -> [a] -> Bool
checkInList _ [] = False
checkInList a (b:bs) | a == b = True
                     | otherwise = checkInList a bs

{-
Function takes a string and returns a list of all substrings of a by removing letters from original string
-}

countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays (w : ws) (t : ts)
  | w == t = countWays ws ts + countWays ws (t : ts)
  | countWays ts [w] > 0 = countWays (ws ++ [w]) (t : ts)
  | otherwise = countWays ws (t : ts)

{-
countWays "Haskell" "Hall"
-> countWays "askell" "all" + countWays "askell" "Hall"
-> countWays "skell" "ll" + countWays "skell" "all"
->

  perms :: String -> [String]
perms [] = [[]]
perms [x] = [[x]]
perms [x, y] = [[x], [y], [x, y]]
perms (x:xs) = join x $ perms xs

join :: Char -> [String] -> [String]
join _ [] = []
join a [[]] = [[a]]
join b (x:xs) = add b x : join b xs

add :: Char -> String -> String
add a b = a : b

-}
  -}
{-
countWays :: String -> String -> Integer
countWays [] _ = 0
countWays _ [] = 1
countWays w t = count t (subStrings w)

count :: String -> [String] -> Integer
count _ [] = 0
count a (b:bs) | a == b = 1 + count a bs
               | otherwise = count a bs

subStrings :: String -> [String]
subStrings [] = [""]
subStrings (x : xs) = prependToAll x rest ++ rest
  where
    rest = subStrings xs

prependToAll :: Char -> [String] -> [String]
prependToAll _ [] = []
prependToAll c (y : ys) = (c : y) : prependToAll c ys
-}