{-# LANGUAGE GADTs #-}

module FMCFun where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )

-- use your mind to infer the types, don't cheat!

-- curry takes a "traditional" binary function
-- and returns its currified version
curry :: ((x, y) -> z) -> (x -> y -> z)
curry f x y = f(x, y)

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (x -> y -> z) -> ((x, y) -> z)
uncurry f(x, y) = f x y

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order
flip :: (x -> y -> z) -> (y -> x -> z)
flip f x y = f y x

-- (.) takes two composable functions and returns their composition
(.) :: (y -> z) -> (x -> y) -> (x -> z)
(.) f g x = f (g x)

-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) :: (x -> y) -> (y -> z) -> x -> z
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?
($) :: (x -> y) -> x -> y
f $ x = f x

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f(f a)

-- orbit
orbit = flip iterate

