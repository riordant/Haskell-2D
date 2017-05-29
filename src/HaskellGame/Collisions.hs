module HaskellGame.Collisions where

import Prelude (
                Num(..), Eq(..), Show(..),
                Bool(..), Char(), Int(), Maybe(..),
                (||), (.), otherwise, not, fst, snd, return
               )

import qualified System.Console.ANSI as Console
import qualified Data.List as List
import Data.List ((++), (!!), elem, any, filter, null)

import HaskellGame.Datatypes
import HaskellGame.Graphics
import HaskellGame.Battle
import HaskellGame.Utils.Dictionary
import HaskellGame.GameMonad

{-
  Check if the player's new position would collide with something.
  Return (True, Just x) if there would be a collision with object x.
  Return (True, Nothing) if there would be a collision with a tile or a monster
  Return (False, Nothing) if there would be no collision.
-}

detectCollision :: Point -> GameAction (Bool, Maybe Object)
detectCollision (x, y) = do
  theLevel <- get currentLevel
  let theMap = map theLevel
  let tile = ((tiles theMap) !! y) !! x
  let nonDroppedItems = filter (not . isDroppedItem) (objects theLevel)
  let objectPositions = List.map position nonDroppedItems
  let monsterPositions = List.map position (monsters theLevel)
  if any (== (x, y)) objectPositions then do
    let theObject = List.head (filter ((== (x, y)) . position) (objects theLevel))
    return (True, Just theObject)
  else if (notWalkable tile) || (any (== (x, y)) monsterPositions) then
    return (True, Nothing)
  else
    return (False, Nothing)
  where
    notWalkable Grass = False
    notWalkable _     = True
