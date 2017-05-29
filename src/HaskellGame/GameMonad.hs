module HaskellGame.GameMonad where

import Prelude ( Monad(..), ($) )

import HaskellGame.Datatypes

data GameAction a = GameAction (Scene -> (Scene, a))

run :: GameAction a -> Scene -> (Scene, a)
run (GameAction x) s = x s

get :: (Scene -> a) -> GameAction a
get f = GameAction (\s -> (s, f s))

change :: (Scene -> Scene) -> GameAction ()
change f = GameAction (\s -> (f s, ()))

instance Monad GameAction where
  return x = GameAction (\s -> (s, x))

  first >>= second = GameAction $
                       (\s ->
                         let (s', r) = run first s
                         in run (second r) s')
