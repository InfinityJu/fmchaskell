{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Nil List"
head (x : xs) = x

tail :: [a] -> [a]
tail [] = error "Nil List"
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null xs = False

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
[] ++ xs = xs
(x : xs) ++ ys = x: (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Nil list"
minimum (x : xs) =
  case xs of
    [] -> x
    (y : ys) ->
      if x <= y
        then minimum (x : ys)
        else minimum (y : ys)

maximum :: Ord a => [a] -> a
maximum [] = error "Nil list"
maximum (x : xs) =
  case xs of
    [] -> x
    (y : ys) ->
      if y <= x
        then maximum (x : ys)
        else maximum (y : ys)

take :: Int -> [a] -> [a]
take i [] = error "Insufficient items in list"
take 1 (x : xs) = [x]
take i (x : xs) = x : take (i-1) xs

drop :: Int -> [a] -> [a]
drop i [] = []
drop 0 xs = xs
drop i (x : xs) = drop (i - 1) xs

takeWhile b [] = []
takeWhile b (x : xs) =
  if b x
    then x: takeWhile b xs
    else []

dropWhile b [] = []
dropWhile b (x : xs) =
  if b x
    then dropWhile b xs
    else x : xs

tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init [] = error "Nil List"
init [x] = []
init (x : xs) = x: init xs

inits [] = error "Nil List"
inits [x] = [[]]
inits xs = snoc (init xs) (inits (init xs))

-- subsequences

any b [] = False
any b [x] = b x
any b (x : xs) = b x || any b xs

all b [] = error "Nil List"
all b [x] = b x
all b (x : xs) = b x && all b xs

and [] = True
and [b] = b
and (b : bs) = b && and bs

or [] = False
or [b] = b
or (b : bs) = b || or bs

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

