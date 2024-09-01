module PrincipalType (typeA, typeB, typeC, typeD, typeE) where

{- ** THE TASK **

Implement functions with the following types. You are not allowed to use
undefined or error.

Haskell is able to infer the most general type for a function that has been
defined in it.  For example
λ> g x y = x y
λ> :t g
g :: (t1 -> t2) -> t1 -> t2

For the following questions, define a function so that if we were to type it
    into haskell, and ask for its type using ":t" we would get types from A, B,
    C, D, and E, up to renaming variables.

This task is worth 10 POINTS.

-}

-- Easy
-- typeA :: (a -> b, a) -> b
typeA (x, y) = x y
{- 
Given in Lecture 
-}


-- Easy
-- typeB :: ((a, a) -> [b]) -> a -> (a, [b])
typeB x y = (y, tail $ x (y, y))
{- 
Two a's give a list of b's. Argument 2 is of type a, so we pass it twice to
argument 1 to get a list of b's. We then take pair it with argument 2 to get
the tuple, and take tail of b to keep b as a list.
-}


-- Medium
-- typeC :: (a -> b -> c -> d -> r) -> ((a, b) -> (c, d) -> r)
typeC x (y, z) (a, b) = x y z a b
{- 
Type is same as (a -> b -> c -> d -> r) -> (a, b) -> (c, d) -> r, so we can
just take argument 1 and pass the two tuples to it.
-}


-- Medium
-- typeD :: (c -> a -> b) -> (c -> a) -> (c -> b)
typeD x y z = x z (y z)
{- 
First argument takes t1 and t2 and returns t3. Second argument takes t1 and
returns t2. So we give third argument (t1) to the second argument to get t2, 
and then give t1 and t2 to the first argument to get t3.
-}


-- Hard
-- typeE :: (a -> b) -> ((a -> c) -> d) -> ((b -> c) -> [d])
-- typeE x y z = map (y . ($ z)) x
typeE a b c = [b ((.) c a)]
{- 
Type is same as (a -> b) -> ((a -> c) -> d) -> (b -> c) -> [d]. Arg 2 is a 
function which takes the compostion of arg 1 and arg 3, and return d. We can
compose arg 1 and arg3, and pass it to arg 2 to get type d. We then use [] to
make it a list.
-}