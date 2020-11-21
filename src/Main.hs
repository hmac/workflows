{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Identity
import Data.Profunctor
import Prelude hiding (id, (.))

main :: IO ()
main = putStrLn "hi"

-- Computation as workflows
--
-- A workflow is a block of computation that takes some input and produces some output
-- It may also perform some effects in the process
-- A workflow can be plugged into other workflows to produce bigger ones, as
-- long as the inputs and outputs line up.
-- When plugged together, we can still inspect the structure of a workflow and
-- determine what sub-worflows it has (in this way it differs from a function)

-- A workflow can be thought of as a tree with Pure at the leaves and Compose,
-- Split and Fork at the branches.
-- Are any of these constructors redundant?

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

-- and an Arrow
instance Arrow W where
  arr = Pure
  first (Pure f) = Pure $ first f
  first w = Split id id w (Pure id)

-- Workflows are able to branch, performing different actions depending on the input they are given

instance ArrowChoice W where
  left w = Fork id (Compose (Pure Left) w) (Pure Right)

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

-- Some simple static analysis

steps :: W i o -> Int
steps (Pure _) = 1
steps (Compose f g) = 1 + steps f + steps g
steps (Split _ _ left right) = 1 + steps left + steps right
steps (Fork _ left right) = 1 + steps left + steps right

depth :: W i o -> Int
depth (Pure _) = 1
depth (Compose f g) = 1 + (depth f) + (depth g)
depth (Split _ _ l r) = 1 + max (depth l) (depth r)
depth (Fork _ l r) = 1 + max (depth l) (depth r)

-- A simple lens type
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

view :: Lens s t a b -> s -> a
view l = getConst . l Const

set :: Lens s t a b -> b -> s -> t
set l b s = runIdentity $ l (const (Identity b)) s

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter afb s = setter s <$> afb (getter s)

splitView :: Lens s t a b -> s -> (a, b -> t)
splitView l s = (view l s, \b -> set l b s)

-- Using a lens we can focus a workflow on a particular component of the input
focus :: Lens s t a b -> W a b -> W s t
focus l w = Split (splitView l) (\(b, f) -> f b) w id

_1 :: Lens (a, c) (b, c) a b
_1 = lens fst (\(x, y) z -> (z, y))

example4 :: W (Int, String) (Bool, String)
example4 = focus _1 example2

petrify :: W a b -> W () ()
petrify (Pure f) = Pure (const ())
petrify (Compose a b) = Compose (petrify a) (petrify b)
petrify (Split split join a b) = Split (const ((), ())) (const ()) (petrify a) (petrify b)
petrify (Fork split a b) = Fork (const (Right ())) (petrify a) (petrify b)

instance Show (W () ()) where
  show (Pure f) = "Pure"
  show (Compose f g) = "(" <> show (petrify f) <> " . " <> show (petrify g) <> ")"
  show (Split _ _ f g) = "(Split " <> show (petrify f) <> " " <> show (petrify g) <> ")"
  show (Fork _ f g) = "(Fork " <> show (petrify f) <> " " <> show (petrify g) <> ")"

-- Can we use this to label workflows?
-- Not sure - we don't want labels to be visible to the workflows themselves.
-- Simple thing is to store the label in the W type but that doesn't seem very extensible...

-- We should be able to use arrow notation
example5 :: W Int Bool
example5 = proc x -> do
  y <- Pure (* 2) -< x
  z <- example2 -< y
  returnA -< z

notW :: W Bool Bool
notW = Fork (\b -> if b then Left () else Right ()) (Pure (const False)) (Pure (const True))

example6 :: W (Int, Int) Bool
example6 = proc (x, y) ->
  if x > y then notW -< True else id -< True
