{-# LANGUAGE ScopedTypeVariables #-}

module Graph (toGraph, toString) where

import Text.Dot
import W

toString :: Dot a -> String
toString = showDot

toGraph :: (Show l, Monoid l, Applicative f) => W l f i o -> Dot (NodeId, NodeId)
toGraph = \case
  Pure l f -> do
    n <- node [("label", show l)]
    pure (n, n)
  Compose l f g -> do
    (f_in, f_out) <- toGraph f
    (g_in, g_out) <- toGraph g
    edge g_out f_in [("label", show l)]
    pure (g_in, f_out)
  Split l _ _ f g -> subgraph [("label", show l)] $ do
    in_ <- node [("label", show l)]
    out_ <- node []
    (f_in, f_out) <- toGraph f
    (g_in, g_out) <- toGraph g
    in_ .->. f_in
    in_ .->. g_in
    f_out .->. out_
    g_out .->. out_
    pure (in_, out_)
  Fork l cond f g -> subgraph [("label", show l)] $ do
    (_, in_) <- toGraph cond
    out_ <- node []
    (f_in, f_out) <- toGraph f
    (g_in, g_out) <- toGraph g
    in_ .->. f_in
    in_ .->. g_in
    f_out .->. out_
    g_out .->. out_
    pure (in_, out_)

subgraph :: [(String, String)] -> Dot a -> Dot a
subgraph attrs m = do
  (_, x) <- cluster $ do
    mapM attribute attrs
    m
  pure x
