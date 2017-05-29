module HaskellGame.Interaction where

import Prelude ( Eq(..), Char(), String(), return, elem, otherwise )
import HaskellGame.Datatypes
import HaskellGame.GameMonad
import HaskellGame.Actions

{- Handle a key press from the player -}

handleInput :: Char -> Command -> GameAction ()
handleInput key cmd
  | key `elem` ['i', 'j', 'k', 'l'] = movePlayer key
  | key == 'a'                      = doAttack
  | key == 'p'                      = pickUpItem
  | key == 'd'                      = dropItem
  | key == '`'                      = doCommand cmd
  | otherwise                       = return ()
