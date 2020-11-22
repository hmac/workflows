{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Data.Functor.Identity
import Data.Profunctor
import Data.String (IsString (fromString))
import Graph
import Lens
import W
import Prelude hiding (even, id, (.), not)
import qualified Prelude

main :: IO ()
main = putStrLn "hi"

type W' = W Label Identity

newtype Label = Label (Maybe String)
  deriving (Semigroup, Monoid)

instance Show Label where
  show (Label (Just l)) = l
  show (Label Nothing) = ""

instance IsString Label where
  fromString = Label . Just

even :: W' Int Bool
even = label "even" (arr Prelude.even)

bool :: W' Bool (Either () ())
bool = label "bool" $ arr $ \x -> if x then Right () else Left ()

double :: W' Int Int
double = label "double" $ arr (*2)

and :: W' (Bool,Bool) Bool
and = label "and" $ arr $ \(x,y) -> x && y

or :: W' (Bool, Bool) Bool
or = label "or" $ arr $ \(x,y) -> x || y

xor :: W' (Bool, Bool) Bool
xor = label "xor" $ arr $ \(x, y) -> if x then if y then False else True else if y then True else False

not :: W' Bool Bool
not =
  label "not" $ arr $ \x -> if x then False else True

example1 :: W' Int Bool
example1 = even . label "double" (arr (* 2))

example2 :: W' Int Bool
example2 =
  fork
    (label "is the number even?" (even >>> bool))
    (arr (const False))
    (arr (const True))

example3 :: W' (Int, Int) Bool
example3 = split id (uncurry (+)) id id >>> even

example4 :: W' (Int, String) (Bool, String)
example4 = focus _1 example2

-- We can use arrow notation
example5 :: W' Int Bool
example5 = proc x -> do
  y <- double -< x
  z <- example2 -< y
  returnA -< z

example6 :: W' (Int, Int) Bool
example6 = proc (x, y) ->
  if x > y then not -< True else id -< True

example7 :: W' Bool Bool
example7 = split (\x -> (x,x)) id (not >>> not) id >>> xor
