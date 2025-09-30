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

    O == O = True
    O == j = False
    j == O = False
    (S j) == (S u) = j == u

instance Ord Nat where

    O <= O = True
    O <= j = True
    j <= O = False
    (S j) <= (S u) = j <= u

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O O = O
    min O j = O
    min j O = O
    min (S j) (S u) = S (min j u)


    max O O = O
    max O j = j
    max j O = j
    max (S j) (S u) = S (max j u)

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
isZero j = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S j) = j
pred zero = zero

even :: Nat -> Bool
even O = True
even (S O) = False
even (S(S j)) = even j

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S j)) = odd j


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
j <+> O = j
j <+> (S u) = S (j <+> u)

infixl 1 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus j O = j
monus O j = O
monus (S j) (S u) = monus j u

(-*) :: Nat -> Nat -> Nat
(-*) = monus

(<->) :: Nat -> Nat -> Nat
(<->) = monus

infixl 1 <->

-- multiplication
times :: Nat -> Nat -> Nat
(times) j  O = O
(times) j (S O) = j
(times) j (S u) = times j u <+> j

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 2 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
(pow) j O = S O
(pow) (S O) j = S O
(pow) j (S u) = pow j u <*> j

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

infixl 3 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
O </> j = O
j </> u =
  case u of
    O -> undefined
    S l ->
      case monus j u of
        O ->
          case monus u j of
            O -> one
            i -> O
        S a -> S ((j -* u) </> u)

infixl 2 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
O <%> j = O
j <%> u =
  case u of
    O -> undefined
    S l ->
      case monus j u of
        O ->
          case monus u j of
            O -> O
            i -> j
        S a -> (j -* u) <%> u

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (j, u) = (j </> u, j <%> u)

-- divides
(<|>) :: Nat -> Nat -> Bool
j <|> u =
  case j of
    O -> undefined
    l ->
      case u <%> j of
        O -> True
        i -> False

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist O O = O
dist O j = j
dist j O = j
dist (S j) (S u) = dist j u

(|-|) = dist

factorial :: Nat -> Nat
factorial O = one
factorial (S O) = one
factorial (S j) = S j <*> factorial j

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg j = S O

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