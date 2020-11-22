{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module W (W (..), label, fork, split, run, focus, _1) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad ((<=<))
import Data.Profunctor
import Data.Profunctor.Choice
import Data.Profunctor.Strong
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

-- Should labels only be on Pure nodes?
data W l f i o
  = -- A single function from input to ouput
    Pure l (i -> f o)
  | -- A composition of two workflows
    forall a. Compose l (W l f a o) (W l f i a)
  | -- Run two workflows in parallel on different parts of the input, joining their outputs
    forall a b c d. Split l (i -> (a, b)) ((c, d) -> o) (W l f a c) (W l f b d)
  | -- Run one of two workflows, based on the input
    forall a b. Fork l (W l f i (Either a b)) (W l f a o) (W l f b o)

label :: l -> W l f i o -> W l f i o
label l (Pure _ f) = Pure l f
label l (Compose _ f g) = Compose l f g
label l (Split _ split join left right) = Split l split join left right
label l (Fork _ split left right) = Fork l split left right

-- Each constructor corresponds to an ability represented by an existing
-- profunctor 'transformer':
-- Pure    ~ Star
-- Compose ~ Procompose
-- Split   ~ Pastro (ish)
-- Fork    ~ PastroSum (ish)

-- i -> o is a profunctor, so W is a profunctor (if f is a functor)
instance (Monoid l, Applicative f) => Profunctor (W l f) where
  dimap f g (Pure l c) = Pure l $ dimap f (fmap g) c
  dimap f g (Compose l a b) = Compose l (rmap g a) (lmap f b)
  dimap f g (Split l split join left right) = Split l (split . f) (g . join) left right
  dimap f g (Fork l cond left right) = Fork l (cond . arr f) (rmap g left) (rmap g right)

instance (Monoid l, Applicative f) => Functor (W l f i) where
  fmap = rmap

instance (Monoid l, Applicative f) => Applicative (W l f i) where
  pure = Pure mempty . const . pure
  wf <*> wx = Split mempty (\i -> (i, i)) (\(f, x) -> f x) wf wx

-- If f is an applicative functor, W is a strong profunctor
instance (Monoid l, Applicative f) => Strong (W l f) where
  first' w = Split mempty id id w id

-- ...with the ability to make choices
instance (Monoid l, Applicative f) => Choice (W l f) where
  left' w = Fork mempty id (rmap Left w) (rmap Right id)

-- but it does not appear to be a closed profunctor
-- instance Applicative f => Closed (W l f)

-- Similarly, W is a Category
instance (Monoid l, Applicative f) => Category (W l f) where
  id = Pure mempty pure
  (.) = Compose mempty

-- and an Arrow
instance (Monoid l, Applicative f) => Arrow (W l f) where
  arr f = Pure mempty (pure . f)
  first w = Split mempty id id w id

-- Workflows are able to branch, performing different actions depending on the input they are given

instance (Monoid l, Applicative f) => ArrowChoice (W l f) where
  left = left'

fork :: (Monoid l, Applicative f) => (W l f a (Either b c)) -> W l f b d -> W l f c d -> W l f a d
fork = Fork mempty

split :: (Monoid l, Applicative f) => (a -> (b, c)) -> ((e, h) -> d) -> W l f b e -> W l f c h -> W l f a d
split = Split mempty

-- All the power of the constructors is represented in the typeclass instances
fork_ :: (Monoid l, Applicative f) => (W l f a (Either b c)) -> W l f b d -> W l f c d -> W l f a d
fork_ split l r = rmap (either id id) (split >>> left' l >>> right' r)

split_ :: (Monoid l, Applicative f) => (a -> (b, c)) -> ((e, h) -> d) -> W l f b e -> W l f c h -> W l f a d
split_ pair unpair l r =
  arr pair >>> first' l >>> second' r >>> arr unpair

-- In order to compose workflows it seems like we need f to be a Monad
run :: Monad f => W l f i o -> i -> f o
run (Pure _ f) = f
run (Compose _ f g) = run f <=< run g
run (Split _ split join left right) = \x ->
  let (l, r) = split x
   in curry join <$> run left l <*> run right r
run (Fork _ cond left right) = \x ->
  run cond x >>= \case
    Left l -> run left l
    Right r -> run right r

-- Some simple static analysis

steps :: W l f i o -> Int
steps (Pure _ _) = 1
steps (Compose _ f g) = 1 + steps f + steps g
steps (Split _ _ _ left right) = 1 + steps left + steps right
steps (Fork _ _ left right) = 1 + steps left + steps right

depth :: W l f i o -> Int
depth (Pure _ _) = 1
depth (Compose _ f g) = 1 + (depth f) + (depth g)
depth (Split _ _ _ l r) = 1 + max (depth l) (depth r)
depth (Fork _ _ l r) = 1 + max (depth l) (depth r)

instance Show l => Show (W l f i o) where
  show (Pure l _) = show l
  show (Compose l f g) = "([" <> show l <> "] " <> show f <> " . " <> show g <> ")"
  show (Split l _ _ f g) = "(parallel [" <> show l <> "] " <> show f <> " " <> show g <> ")"
  show (Fork l cond f g) = "(" <> show l <> " if " <> show cond <> " then " <> show f <> " else " <> show g <> ")"

-- Can we use this to label workflows?
-- Not sure - we don't want labels to be visible to the workflows themselves.
-- Simple thing is to store the label in the W type but that doesn't seem very
-- extensible...

-- Using a lens we can focus a workflow on a particular component of the input
focus :: (Monoid l, Applicative f) => Lens s t a b -> W l f a b -> W l f s t
focus l w = Split mempty (splitView l) (\(b, f) -> f b) w id
