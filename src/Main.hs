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
import Prelude hiding (id, (.))

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

example1 :: W' Int Bool
example1 = label "even" (arr even) . label "double" (arr (* 2))

example2 :: W' Int Bool
example2 =
  let w1 = left' (arr (const False))
      w2 = right' (arr (const True))
      w3 = arr (\x -> if even x then Right () else Left ())
   in (rmap (either id id) (w3 >>> w1 >>> w2)) . arr (* 2)

example3 :: (Monoid l, Applicative f) => W l f (Int, Int) Bool
example3 = split id (uncurry (+)) id id >>> arr even

example4 :: W' (Int, String) (Bool, String)
example4 = focus _1 example2

-- We can use arrow notation
example5 :: W' Int Bool
example5 = proc x -> do
  y <- arr (* 2) -< x
  z <- example2 -< y
  returnA -< z

notW :: (Monoid l, Applicative f) => W l f Bool Bool
notW =
  fork
    (\b -> if b then Left () else Right ())
    (arr (const False))
    (arr (const True))

example6 :: (Monoid l, Applicative f) => W l f (Int, Int) Bool
example6 = proc (x, y) ->
  if x > y then notW -< True else id -< True
