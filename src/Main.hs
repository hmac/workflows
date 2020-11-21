{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Arrow
import Control.Category
import Data.Profunctor
import Prelude hiding (id, (.))

main :: IO ()
main = putStrLn "hi"

-- Computation as workflows
--
-- A workflow is a block of computation that takes some input and produces some output
-- It may also perform some effects in the process
-- A workflow can be plugged into other workflows to produce bigger ones, as long as the inputs and outputs line up.
-- When plugged together, we can still inspect the structure of a workflow and determine what sub-worflows it has (in this way it differs from a function)

data W i o
  = -- A single function from input to ouput
    Pure (i -> o)
  | -- A composition of two workflows
    forall j. Compose (W j o) (W i j)
  | -- Run two workflows in parallel on different parts of the input, joining their outputs
    forall i1 i2 o1 o2. Split (i -> (i1, i2)) ((o1, o2) -> o) (W i1 o1) (W i2 o2)
  | -- Run one of two workflows, based on the input
    forall i1 i2. Fork (i -> Either i1 i2) (W i1 o) (W i2 o)

-- i -> o is a profunctor, so W is a profunctor
instance Profunctor W where
  dimap f g (Pure c) = Pure $ g . c . f
  dimap f g (Compose a b) = Compose (rmap g a) (lmap f b)
  dimap f g (Split split join left right) = Split (split . f) (g . join) left right
  dimap f g (Fork cond left right) = Fork (cond . f) (Pure g . left) (Pure g . right)

-- Similarly, W is a Category
instance Category W where
  id = Pure id
  (.) = Compose

-- And an Arrow
instance Arrow W where
  arr = Pure
  first (Pure f) = Pure $ first f
  first w = Split id id w (Pure id)

-- Workflows are able to branch, performing different actions depending on the input they are given

instance ArrowChoice W where
  left w = Fork id (Compose (Pure Left) w) (Pure Right)

-- A workflow can be thought of as a tree with Pure at the leaves and Compose, Split and Fork at the branches.

run :: W i o -> i -> o
run (Pure f) = f
run (Compose f g) = run f . run g
run (Split split join left right) = \x ->
  let (l, r) = split x
   in join (run left l, run right r)
run (Fork cond left right) = \x -> case cond x of
  Left l -> run left l
  Right r -> run right r

example1 :: W Int Bool
example1 = Compose (Pure even) (Pure (* 2))

example2 :: W Int Bool
example2 =
  Compose
    ( Fork
        (\x -> if even x then Right () else Left ())
        (Pure (const False))
        (Pure (const True))
    )
    (Pure (* 2))

example3 :: W (Int, Int) Bool
example3 = Compose (arr even) (Split id (uncurry (+)) id id)

steps :: W i o -> Int
steps (Pure _) = 1
steps (Compose f g) = 1 + steps f + steps g
steps (Split _ _ left right) = 1 + steps left + steps right
steps (Fork _ left right) = 1 + steps left + steps right
