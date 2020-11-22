module Lens where

import Control.Applicative
import Data.Functor.Identity

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

_1 :: Lens (a, c) (b, c) a b
_1 = lens fst (\(x, y) z -> (z, y))
