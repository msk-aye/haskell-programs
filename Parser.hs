module Parser 
    (Polynomial(..), Parser (..), polynomial, parse, expand, simplify) where

import           Control.Applicative

{--
Do not import anything besides Control.Applicative.
You are allowed to use anything available in Prelude and Control.Applicative,
as well as any syntax features.

====
TASK
====

Write an instance of show for the Polynomial data type that produces *exactly*
the behaviour in the example.

In particular, only print brackets when necessary.

Note that we're allowing zero coefficients on monos to simplify the parser.

=======
EXAMPLE
=======
> Mono 1 1
x

> Mono 1 0
1

> (m,n,j,k) = (Mono 1 2, Mono 3 4, Mono 5 6, Mono 7 8)
(x^2,3x^4,5x^6,7x^8)

> Add (m) (Add (n) (j))
x^2 + 3x^4 + 5x^6

> Add (Add (m) (n)) (j)
x^2 + 3x^4 + 5x^6

> Mul (Add (m) (n)) (Add (j) (k))
(x^2 + 3x^4)(5x^6 + 7x^8)

> Mul m n
(x^2)(3x^4)

> Mul (Mul m n) j
(x^2)(3x^4)(5x^6)

> Mul m (Mul n j)
(x^2)(3x^4)(5x^6)

====
TASK
====

Write a parser
    polynomial :: Parser Polynomial
for reading the string representation of a polynomial back into the Polynomial
data type.

Use polynomial to define an instance of read for the Polynomial data type.

=======
EXAMPLE
=======
> (parse polynomial) ")("
Nothing

> (parse polynomial) "2x^3"

> (parse polynomial) "(2x^3)"
Just (Mono 2 3,"")

> (parse polynomial) ("0x^2")  -- It's okay to do this
Just (Mono 0 2,"")

> (parse polynomial) ("3x^24x^3")
Just (Mul (Mono 3 24) (Mono 1 3),"")

> (parse polynomial) "(2x^2+3)(x^3)"
Just (Mul (Add (Mono 2 2) (Mono 3 0)) (Mono 1 3),"")

> (parse polynomial) "(1+x^2)+x^3"
Just (Add (Add (Mono 1 0) (Mono 1 2)) (Mono 1 3),"")

> (parse polynomial) "1+(x^2+x^3)"
Just (Add (Mono 1 0) (Add (Mono 1 2) (Mono 1 3)),"")

> (parse polynomial) "(x)(x^2)+x^3"
Just (Add (Mul (Mono 1 1) (Mono 1 2)) (Mono 1 3),"")

> (parse polynomial) "(x)(x)(x)"
Just (Mul (Mono 1 1) (Mul (Mono 1 1) (Mono 1 1)),"")

> (parse polynomial) "(((x)))"
Just (Mono 1 1,"")

This task is worth 10 POINTS.
--}

--  start: DO NOT MODIFY --

type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always nonnegative.

data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial 
                                 | Mul Polynomial Polynomial deriving (Eq)

-- expand and simplify from A2

expand :: Polynomial -> Polynomial
expand (Mono c d) = Mono c d
expand (Add f g) = Add (expand f) (expand g)
expand (Mul (Mono c0 d0) (Mono c1 d1)) = Mono (c0*c1) (d0+d1)
expand (Mul (Add f g) h) = Add (expand $ Mul f h) (expand $ Mul g h)  -- right
expand (Mul f (Add g h)) = Add (expand $ Mul f g) (expand $ Mul f h)  -- left
expand (Mul f g) = expand $ Mul (expand f) (expand g)


-- simplified polynomial is returned in descending degree

simplify :: Polynomial -> Polynomial
simplify (Mono c d) = Mono c d
simplify (Add g h)  = merge' (simplify g) (simplify h)
simplify f          = simplify $ expand f


-- Precondition: input is simplified

merge' :: Polynomial -> Polynomial -> Polynomial
merge' (Mono a b) (Mono c d)
    | b > d      = Add (Mono a b) (Mono c d)
    | d > b      = Add (Mono c d) (Mono a b)
    | otherwise  = Mono (a+c) d
merge' (Mono lcf df) g
    | df > dg   = Add (Mono lcf df) g
    | dg > df   = Add (Mono lcg dg) $ merge' (Mono lcf df) gt
    | otherwise = Add (Mono (lcf+lcg) df) gt
  where
    Add (Mono lcg dg) gt = g
merge' f (Mono c d) = merge' (Mono c d) f
merge' f g
    | df > dg   = Add (Mono lcf df) (merge' ft g)
    | dg > df   = Add (Mono lcg dg) (merge' gt f)
    | otherwise = Add (Mono (lcf+lcg) df) (merge' ft gt)
  where
    Add (Mono lcf df) ft = f
    Add (Mono lcg dg) gt = g


-- Parser type

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap g pa = do
      a <- pa
      return $ g a

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\cs -> Just (a,cs))

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> pa = do
      g <- pg
      g <$> pa

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \cs ->
        case parse p cs of
          Nothing        -> Nothing
          Just (a, str') -> parse (f a) str'

instance Alternative Parser where
    empty :: Parser a
    empty = P $ \str -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \cs ->
        case parse p cs of
          Nothing -> parse q cs
          mx      -> mx

-- aux function for removing decorator
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) cs = p cs

-- parase one character
item :: Parser Char
item = P $ foo
  where
    foo (c:cs) = Just $ (c, cs)
    foo _      = Nothing

-- parse a char c when P c.
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

-- parse a digit
digit :: Parser Char
digit = sat (\x -> elem x ['0'..'9'])

-- parse the character x
char :: Char -> Parser Char
char x = sat (== x)

-- parse the string xs
string :: String -> Parser String
string []     = return []
string (x:xs) = (\x xs -> x:xs) <$> (char x) <*> (string xs)

-- parse a natural number
nat :: Parser Integer
nat = read <$> (some digit)

-- throw away space
space :: Parser ()
space = (\x -> ()) <$> (many $ char ' ')

-- ignore surrounding whitespace
token :: Parser a -> Parser a
token pa = do
    space
    a <- pa
    space
    return a

-- parse a symbol, ignoring whitespace
symbol :: String -> Parser String
symbol xs = token $ string xs

-- end DO NOT MODIFY --
-- Your code goes below

-- Polynom ::= Factors "+" Polynom | Factors
--
-- Factors ::= Factor Factors | Factor
--
-- Factor ::= "(" Polynom ")" | Mono
--
-- Mono ::= Constant "x" "^" Constant
--          | "x" "^" Constant
--          | Constant "x"
--          | "x"
--          | Constant
--
-- Constant ::= 0 | 1 | 2 | ...

instance Show Polynomial where
    show (Mono c 0) = show c
    show (Mono 0 _) = show 0
    show (Mono 1 1) = "x"
    show (Mono c 1) = show c ++ "x"
    show (Mono 1 d) = "x^" ++ show d
    show (Mono c d) = show c ++ "x^" ++ show d
    show (Add f g) = show f ++ " + " ++ show g
    show (Mul (Mul f a) (Mul g b)) = show (Mul f a) ++ show (Mul g b)
    show (Mul (Mul f a) g) = show (Mul f a) ++ "(" ++ show g ++ ")"
    show (Mul f (Mul g a)) = "(" ++ show f ++ ")" ++ show (Mul g a)
    show (Mul f g) = "(" ++ show f ++ ")" ++ "(" ++ show g ++ ")"

-- Polynom ::= Factors "+" Polynom | Factors
polynomial :: Parser Polynomial
polynomial = do
    f <- factors
    token $ char '+'
    p <- polynomial
    return $ Add f p
    <|>
    factors

-- Factors ::= Factor Factors | Factor
factors :: Parser Polynomial
factors = do
    f <- factor
    fs <- factors
    return $ Mul f fs
    <|> 
    factor

-- Factor ::= "(" Polynom ")" | Mono
factor :: Parser Polynomial
factor = do
    token $ char '('
    p <- polynomial
    token $ char ')'
    return p
    <|>
    mono

-- Mono ::= Constant "x" "^" Constant | "x" "^" Constant
--          | Constant "x" | "x" | Constant
mono :: Parser Polynomial
mono = do
    c <- token nat
    token $ char 'x'
    token $ char '^'
    d <- token nat
    return $ Mono c d
    <|>
    do
      token $ char 'x'
      token $ char '^'
      d <- token nat
      return $ Mono 1 d
    <|>
    do
      c <- token nat
      token $ char 'x'
      return $ Mono c 1
    <|>
    do
      token $ char 'x'
      return $ Mono 1 1
    <|>
    constant

constant :: Parser Polynomial
constant = do
  x <- token nat
  return $ Mono x 0