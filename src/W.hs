{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}

module W (W, fork, split, run, focus, _1) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Data.Profunctor
import Lens
import Prelude hiding (id, (.))

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

data W f i o
  = -- A single function from input to ouput
    Pure (i -> f o)
  | -- A composition of two workflows
    forall j. Compose (W f j o) (W f i j)
  | -- Run two workflows in parallel on different parts of the input, joining their outputs
    forall i1 i2 o1 o2. Split (i -> (i1, i2)) ((o1, o2) -> o) (W f i1 o1) (W f i2 o2)
  | -- Run one of two workflows, based on the input
    forall i1 i2. Fork (i -> Either i1 i2) (W f i1 o) (W f i2 o)

-- Each constructor corresponds to an ability represented by an existing
-- profunctor 'transformer':
-- Pure    ~ Star
-- Compose ~ ?
-- Split   ~ Pastro (ish)
-- Fork    ~ PastroSum (ish)

-- i -> o is a profunctor, so W is a profunctor (if f is a functor)
instance Functor f => Profunctor (W f) where
  dimap f g (Pure c) = Pure $ dimap f (fmap g) c
  dimap f g (Compose a b) = Compose (rmap g a) (lmap f b)
  dimap f g (Split split join left right) = Split (split . f) (g . join) left right
  dimap f g (Fork cond left right) = Fork (cond . f) (rmap g left) (rmap g right)

instance Functor f => Functor (W f i) where
  fmap = rmap

instance Applicative f => Applicative (W f i) where
  pure = Pure . const . pure
  wf <*> wx = Split (\i -> (i, i)) (\(f, x) -> f x) wf wx

-- If f is an applicative functor, W is a strong profunctor
instance Applicative f => Strong (W f) where
  first' w = Split id id w id

-- ...with the ability to make choices
instance Applicative f => Choice (W f) where
  left' w = Fork id (rmap Left w) (rmap Right id)

-- but it does not appear to be a closed profunctor
-- instance Applicative f => Closed (W f)

-- Similarly, W is a Category
instance Applicative f => Category (W f) where
  id = Pure pure
  (.) = Compose

-- and an Arrow
instance Applicative f => Arrow (W f) where
  arr f = Pure (pure . f)
  first w = Split id id w id

-- Workflows are able to branch, performing different actions depending on the input they are given

instance Applicative f => ArrowChoice (W f) where
  left = left'

-- All the power of the constructors is represented in the typeclass instances
fork :: Applicative f => (a -> Either b c) -> W f b d -> W f c d -> W f a d
fork split l r = rmap (either id id) (arr split >>> left' l >>> right' r)

split :: Applicative f => (a -> (b, c)) -> ((e, h) -> d) -> W f b e -> W f c h -> W f a d
split pair unpair l r =
  arr pair >>> first' l >>> second' r >>> arr unpair

-- In order to compose workflows it seems like we need f to be a Monad
run :: Monad f => W f i o -> i -> f o
run (Pure f) = f
run (Compose f g) = run f <=< run g
run (Split split join left right) = \x ->
  let (l, r) = split x
   in curry join <$> run left l <*> run right r
run (Fork cond left right) = \x -> case cond x of
  Left l -> run left l
  Right r -> run right r

-- Some simple static analysis

steps :: W f i o -> Int
steps (Pure _) = 1
steps (Compose f g) = 1 + steps f + steps g
steps (Split _ _ left right) = 1 + steps left + steps right
steps (Fork _ left right) = 1 + steps left + steps right

depth :: W f i o -> Int
depth (Pure _) = 1
depth (Compose f g) = 1 + (depth f) + (depth g)
depth (Split _ _ l r) = 1 + max (depth l) (depth r)
depth (Fork _ l r) = 1 + max (depth l) (depth r)

petrify :: Applicative f => W f a b -> W f () ()
petrify (Pure f) = Pure (pure . const ())
petrify (Compose a b) = Compose (petrify a) (petrify b)
petrify (Split split join a b) = Split (const ((), ())) (const ()) (petrify a) (petrify b)
petrify (Fork split a b) = Fork (const (Right ())) (petrify a) (petrify b)

instance Applicative f => Show (W f () ()) where
  show (Pure f) = "Pure"
  show (Compose f g) = "(" <> show (petrify f) <> " . " <> show (petrify g) <> ")"
  show (Split _ _ f g) = "(Split " <> show (petrify f) <> " " <> show (petrify g) <> ")"
  show (Fork _ f g) = "(Fork " <> show (petrify f) <> " " <> show (petrify g) <> ")"

-- Can we use this to label workflows?
-- Not sure - we don't want labels to be visible to the workflows themselves.
-- Simple thing is to store the label in the W type but that doesn't seem very
-- extensible...

-- Using a lens we can focus a workflow on a particular component of the input
focus :: Applicative f => Lens s t a b -> W f a b -> W f s t
focus l w = Split (splitView l) (\(b, f) -> f b) w id
