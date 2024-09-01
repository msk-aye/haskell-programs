module FirstCommon (firstCommon) where


import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

{-

You are not allowed to use
    - list comprehensions
    - do-notation
    - any imports
    - any zips or unzips
    - any maps and their operator counterparts (including, but not limited to 
      fmap, amap, mapM, mapM_)
    - any functions on lists and their operator counterparts (including but 
      not limited to sorts, folds, filters, reverse, replicate)
    - (<$>), (<*>), (*>). (>>=), (>>).

Gradescope will check your submission for any of the prohibited syntax, 
functions, operators and imports.

** Basically, the rule is: if you want to use something, implement it yourself!

Execution time and memory consumption are limited.

Memory consumption is limited to 4 GB.

Execution time is limited to 10 minutes for the whole submission. Some tests may
have their own time limits. If a single timed test exceeds the limit, only that
test will fail. If the whole submission takes longer than 10 minutes to run, 
you will receive 0 for the whole submission.

-}



-- ** THE TASK **

-- Implement a function which will find the most common element in a list. 
-- If there are several elements with this property, return the one that is 
-- encountered earlier. 
-- We guarantee that the list will contain at least one element.

-- Examples:
-- firstCommon [1, 2, 3, 1] == 1
-- firstCommon [1] == 1
-- firstCommon [2, 3, 1] == 2
--
-- This task is worth 10 POINTS.

firstCommon :: (Eq a) => [a] -> a
firstCommon [] = undefined
firstCommon [a] = a
firstCommon (a:as) = findMaxx $ freqDict (a:as)

findMaxx :: [(Integer, a)] -> a
findMaxx [] = undefined
findMaxx [(a, b)] = b
findMaxx ((a, b):(c, d):as) | a > c = findMaxx ((a, b):as)
                            | otherwise = findMaxx ((c, d):as)

freqDict :: Eq a => [a] -> [(Integer, a)]
freqDict [] = []
freqDict (c:cs) = join c $ freqDict cs

join :: Eq a => a -> [(Integer, a)] -> [(Integer, a)]
join a [] = [(1, a)]
join a ((n, b):bs) | a == b = (n + 1, b) : bs
                     | otherwise = (n, b) : join a bs