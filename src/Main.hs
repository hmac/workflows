{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Data.Functor.Identity
import Data.Profunctor
import Lens
import W
import Prelude hiding (id, (.))

main :: IO ()
main = putStrLn "hi"

example1 :: Applicative f => W f Int Bool
example1 = arr even . arr (* 2)

example2 :: Applicative f => W f Int Bool
example2 =
  let w1 = left' (arr (const False))
      w2 = right' (arr (const True))
      w3 = arr (\x -> if even x then Right () else Left ())
   in (rmap (either id id) (w3 >>> w1 >>> w2)) . arr (* 2)

example3 :: Applicative f => W f (Int, Int) Bool
example3 = split id (uncurry (+)) id id >>> arr even

example4 :: Applicative f => W f (Int, String) (Bool, String)
example4 = focus _1 example2

-- We can use arrow notation
example5 :: Applicative f => W f Int Bool
example5 = proc x -> do
  y <- arr (* 2) -< x
  z <- example2 -< y
  returnA -< z

notW :: Applicative f => W f Bool Bool
notW =
  fork
    (\b -> if b then Left () else Right ())
    (arr (const False))
    (arr (const True))

example6 :: Applicative f => W f (Int, Int) Bool
example6 = proc (x, y) ->
  if x > y then notW -< True else id -< True
