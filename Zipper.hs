module Zipper (Zipper (..), toZipper, zip1, unzip1, fullyZip, fullyUnzip) where

{--
*DO NOT import any modules*
*You are allowed to use anything available in Prelude and any syntax features*

You are given the data type

    data Zipper a = Zipper [(a, a)] [a] [a]

which represents an actual zipper.

If you look at a real zipper, you'll see that it has some (possily none) pairs
of teeth are connected below the slider, and there are two (possibly empty)
sequences of disconnected teeth above the slider.

            "pairs of connected teeth below the slider"
                               |
    data Zipper a = Zipper [(a, a)] [a] [a]
                                     |   |
             "two sequences of disconnected teeth above the slider"


1. Your first task is make the Zipper a Functor:

    instance Functor Zipper where
        fmap = ...

fmap must abide all Functor laws in your implementation.


2. Implement a function to create a zipper from two lists. They can have
different lengths of be empty. This function should return a completely
"unzipped" Zipper.

    toZipper [] [1, 2, 3]        == Zipper [] [] [1, 2, 3]
    toZipper [4, 5, 6] [1, 2, 3] == Zipper [] [4, 5, 6] [1, 2, 3]
    toZipper [4, 5, 6] []        == Zipper [] [4, 5, 6] []
                                           ||
    The newly-created Zipper is fully unzipped. There are no connected teeth.


3. Implement functions to move the slider in either direction.

        zip1 :: Zipper a -> Zipper a

    This function connects exactly one pair of teeth.
    If either of the lists of disconnected teeth is empty, zip1 should have no
    effect on the Zipper.

        z = toZipper [1, 2] [4, 5, 6]

        zip1 z == Zipper [(1, 4)] [2] [5, 6]
        zip1 (zip1 z) == Zipper [(1, 4), (2, 5)] [] [6]
                -- All further applications of zip1 should have no effect
        zip1 (zip1 (zip1 z)) == Zipper [(1, 4), (2, 5)] [] [6]


        unzip1 :: Zipper a -> Zipper a

    This function is the opposite of zip1. If there is nothing to unzip, it
    should leave the Zipper as it is.

        z = Zipper [(1, 4), (2, 5)] [] [6]
        unzip1 z == Zipper [(1, 4)] [2] [5, 6]
        unzip1 (unzip1 z) == Zipper [] [1, 2] [4, 5, 6]
                -- All further applications of unzip1 should have no effect
        unzip1 (unzip1 (unzip1 z)) == Zipper [] [1, 2] [4, 5, 6] 


        fullyZip :: Zipper a -> Zipper a

    This function completely zips a Zipper to the point where at least one of
    the lists of disconnected teeth is empty.

        fullyZip (Zipper [(1, 2)] [3, 4, 5] [6, 7, 8, 9, 10])
                        == Zipper [(1, 2), (3, 6), (4, 7), (5, 8)] [] [9, 10]


        fullyUnzip :: Zipper a -> Zipper a

    This is the opposite of fullyZip.

        fullyUnzip (Zipper [(1, 2), (3, 6), (4, 7), (5, 8)] [] [9, 10])
                                == Zipper [] [1, 3, 4, 5] [2, 6, 7, 8, 9, 10]


Lists of disconnected teeth might be infinite, so plan your implementation
accordingly.
--}

data Zipper a = Zipper [(a, a)] [a] [a]
    deriving (Eq, Show)

applyTuple :: (a -> b) -> (a, a) -> (b, b)
applyTuple f (x, y) = (f x, f y)

instance Functor Zipper where
    -- fmap :: (a -> b) -> Zipper a -> Zipper b
    -- Rule 1: fmap id
    -- Rule 2: fmap (g.h) = fmap g . fmap h
    fmap f (Zipper a b c) = Zipper (applyTuple f <$> a) (f <$> b) (f <$> c)

toZipper :: [a] -> [a] -> Zipper a
toZipper = Zipper []

zip1 :: Zipper a -> Zipper a
zip1 (Zipper connected [] other) = Zipper connected [] other
zip1 (Zipper connected other []) = Zipper connected other []
zip1 (Zipper connected (x : xs) (y : ys)) = Zipper (connected ++ [(x, y)]) xs ys

unzip1 :: Zipper a -> Zipper a
unzip1 (Zipper [] a b ) = Zipper [] a b
unzip1 (Zipper x a b) = Zipper (init x) (fst (last x) : a) (snd (last x) : b)

fullyZip :: Zipper a -> Zipper a
fullyZip (Zipper connected [] other) = Zipper connected [] other
fullyZip (Zipper connected other []) = Zipper connected other []
fullyZip z = fullyZip (zip1 z)

fullyUnzip :: Zipper a -> Zipper a
fullyUnzip (Zipper [] a b) = Zipper [] a b
fullyUnzip z = fullyUnzip (unzip1 z)