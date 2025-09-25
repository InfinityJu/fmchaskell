{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )
import System.Win32 (xBUTTON1, kEYEVENTF_EXTENDEDKEY)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero x = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S x) = x
pred zero = zero

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S(S x)) = even x

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S x)) = odd x

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus x O = x
monus O x = O
monus (S x) (S y) = monus x y

(-*) :: Nat -> Nat -> Nat
(-*) = monus

infixl 6 -*

-- multiplication
(*) :: Nat -> Nat -> Nat
x * O = O
x * S O = x
x * (S y) = x * y + x

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
x ^ O = one
(S O) ^ x = one
x ^ (S y) = x ^ y * x

infixl 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
x / zero = undefined
zero / x = zero
x / one = x
x / y = 
  case monus x y of
    zero -> 
      case absDiff x y of
        O -> one
        k -> O
    (S z) -> S((x-*y)/y) 

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
zero % x = zero
x % zero = undefined
x % one = zero
x % y =
  case monus x y of
    O ->
      case absDiff x y of
        O -> O
        z -> x
    (S z) -> (x -* y) % y

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
zero ||| x = undefined
one ||| x = S O
y ||| x =
  case x % y of
    zero -> S O
    k -> O

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O O = O 
absDiff O x = x
absDiff x O = x
absDiff (S x) (S y) = absDiff x y

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = one
factorial (S O) = one
factorial (S x) = S x * factorial x

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg x = one
-- isso porque não temos números negativos nos naturais
-- logo, não é possível que o sinal de um número seja -1

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo x one = zero
lo x y =
  case monus x y of
    O -> S(lo x (y / x))
    (S z) -> O