module Tut2PrincipalTypes where

-- q1 :: (a -> [b]) -> [a] -> [b]
q1 f (x:xs) = f x ++ q1 f xs
{- 
To ensure that the second argument is a list, we can pattern match against (x:xs).
To ensure that the first argument, f, takes a scalar of the same inner type `a`
as (x:xs), we pass it x, the head of the list.
To ensure that f returns a list of some sort, we can concatenate it with another list.
We will use q1 itself here, but plenty of other lists could produce a "working" result.
-}

-- q2 :: (a, b) -> (c, d) -> ((b, d), (a, c))
q2 (a, b) (c, d) = ((b, d), (a, c))
{-
Using type matching, we can both ensure that the input types are tuples, and 'extract'
the values inside. Haskell by default will assume that these could be different types.
We then just rearrange the values into a tuple of tuples as our output.
-}

-- q3 :: (a -> b) -> ((a -> c) -> c) -> ((b -> c) -> c)
q3 f g h = g (\x -> g (h . f))
{-
Since the -> operator is right associative, the type signature is the same as
q3 :: (a -> b) -> ((a -> c) -> c) -> (b -> c) -> c
This allows us to work with three inputs, f, g and h, instead of two.
We can use the composition operator on h and f, which ensures that the output type 
of f is the same as the input type of h. We can then think of the composition like 
(h . f) :: a -> c
To ensure that this is the same type as the argument passed to g, we simply pass 
(h . f) to g.
q3 f g h = g (h . f) :: (a -> b) -> ((a -> c) -> t) -> (b -> c) -> t
This definition is almost there, but we need to make those t's into c's
To make sure that the output of g and the output of g's argument are the same
 - define an anonymous function which uses g, and therefore returns something of type c
 - pass that anonymous function to g
-}

-- q4 :: (a -> b -> c) -> (a -> b) -> a -> c
q4 f g a = f a (g a)
{-
Pass a to f to ensure that the first arg of f is the same type as a
Pass a to g for to ensure that the input for g is the same type as well
To make f's second arg the same type as g's output, pass (g a) as the second
argument to f.
-}

-- q5 :: a -> [b] -> a -> ([a] -> [c]) -> c
q5 a0 (b:bs) a1 f = head $ f [a0, a1]
{-
Put a1 and a2 in a list together to make them the same type
We use pattern matching to make the second argument a list.
To make f take a list of a's as input, pass it [a1, a2]
And to make it's output a list, but still return the scalar c, get the head
of f's output
-}

-- q6 :: [a] -> (b, c) -> (b -> [a] -> d) -> d
q6 (a:as) (b, c) f = f b (a:as)
{-
We can make the first argument a list using pattern matching.
We can make the second argument a tuple using pattern matching as well, and get
access to its inner values.
Then, we just pass b and the list of a's to f to ensure that it takes things of
those respective types.
-}

-- q7 :: ((a -> b -> c) -> a) -> (a -> c) -> c
q7 f g = g $ f (\x y -> g x)
{-
We want to use the output from g as the 'final' output, since we want g's and q7's
output types to be the same (c) - so we use g on the outside.
We also know that we want f's output and g's input to be the same, so we'll evaluate
f and use it as input to g.
We want f's argument to be a bivariate function in order to get some output - what can
we use?
 - If g :: a -> c, we can use g somehow to match those c's
 - We will use the anonymous function (\x y -> g x). 
 - We don't care about the second arg, but we want to make sure that the first arg 
    is the same type as the input to g. 
 - We can do this by simply passing the first argument of the anonymous function to g.
-}