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
import Lens
import W
import Prelude hiding (even, id, (.))
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

example1 :: W' Int Bool
example1 = even . label "double" (arr (* 2))

example2 :: W' Int Bool
example2 =
  fork
    (even >>> arr (\x -> if x then Right () else Left ()))
    (arr (const False))
    (arr (const True))

example3 :: W' (Int, Int) Bool
example3 = split id (uncurry (+)) id id >>> even

example4 :: W' (Int, String) (Bool, String)
example4 = focus _1 example2

-- We can use arrow notation
example5 :: W' Int Bool
example5 = proc x -> do
  y <- arr (* 2) -< x
  z <- example2 -< y
  returnA -< z

bool :: W' Bool (Either () ())
bool = arr $ \x -> if x then Right () else Left ()

notW :: W' Bool Bool
notW =
  label "not" $
    fork
      bool
      (arr (const True))
      (arr (const False))

example6 :: W' (Int, Int) Bool
example6 = proc (x, y) ->
  if x > y then notW -< True else id -< True
