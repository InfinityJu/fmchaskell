{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )
import Distribution.Simple.Setup (trueArg, TestFlags (testDistPref))
import Text.XHtml (tt)
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = 'S' : show n

instance Eq Nat where

    (==) O O = True
    (==) O n = False
    (==) n O = False
    (==) (S n) (S m) = (==) n m

instance Ord Nat where

    (<=) O O = True
    (<=) O x = True
    (<=) x O = False
    (<=) (S x) (S y) = (<=) x y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O O = O
    min O x = O
    min x O = O
    min (S x) (S y) = min x y


    max O O = O
    max O x = x
    max x O = x
    max (S x) (S y) = max x y

----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero x = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S x) = x
pred zero = zero

even :: Nat -> Bool
even O = True
even (S O) = False
even (S(S x)) = even x

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S x)) = odd x


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) x O = x
(<+>) x (S y) = S (x + y)

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

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
(times) x  O = O
(times) x (S O) = x
(times) x (S y) = (<+>) (times x y) x

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
(pow) x O = S O
(pow) (S O) x = S O
(pow) x (S y) = pow x y * x

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) O x = O
(</>) x y =
  case y of
    O -> undefined
    S z ->
      case monus x y of
        O ->
          case monus y x of
            O -> one
            w -> O
        S w -> S ((</>) (x -* y) y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) O x = O
(<%>) x y =
  case y of
    O -> undefined
    S z ->
      case monus x y of
        O ->
          case monus y x of
            O -> O
            w -> x
        S w -> (<%>) (x -* y) y

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (x, y) = ((</>) x y, (<%>) x y)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) y x =
  case y of
    O -> undefined
    z ->
      case (<%>) x y of
        O -> True
        w -> False

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist O O = O
dist O x = x
dist x O = x
dist (S x) (S y) = dist x y

(|-|) = dist

factorial :: Nat -> Nat
factorial O = one
factorial (S O) = one
factorial (S x) = S x * factorial x

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg x = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo j u =
  case j of
    O -> undefined
    (S O) -> undefined
    l ->
      case monus u j of
        O ->
          case monus j u of
            O -> one
            S i -> O
        S a -> S (lo j ((</>) u j))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat i =
    if i <= 0
        then undefined
        else S (toNat (i - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

