module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise, tail
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum 0 = False
    toEnum 1 = True

    fromEnum False = 0
    fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && j = j
False && u = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
True || j = True
j || True = True
j || u = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
j /|\ u = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
j \|/ u = False

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True <=/=> j = not j
False <=/=> j = j

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t e = t
ifThenElse False t e = e

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> j = j
False ==> j = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
j <== True = j
j <== False = True

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
True <=> j = j
False <=> j =  not j

infixr 1 <=>