module HaskellGame.Messages where

import Prelude ( Show(..), Eq(..), Ord(..), Num(..),
                 Int() )

import Data.List ((++), head, length, last)

import qualified System.Console.ANSI as Console

import HaskellGame.Datatypes
import HaskellGame.Graphics
import HaskellGame.Utils

{-
   We want to avoid printing the same message over and over,
   so only add a message to the message list if it's different
   to the previous one.
-}

addMessages :: [Message] -> [Message] -> [Message]
addMessages [] x = x
addMessages x [] = x
addMessages old new =
  if (last old) == (head new) then
    addMessages old (dropsome 1 new) -- just drop the new message
  else
    addMessages (old ++ [head new]) (dropsome 1 new)

{-
   We want to make sure only as many as the message limit messages
   are displayed, so we'll remove old ones to get down to the limit.
-}
removeOldMessages :: Int -> [Message] -> [Message]
removeOldMessages limit messages =
  if length messages > limit then
    dropsome ((length messages) - limit) messages
  else messages

{- Actual messages below -}

missedMessage = [(Console.Red, "You flail wildly at empty space! Your attack connects with nothing.")]

hitMessage monster monsterDamage player playerDamage =
  [(Console.Red, [symbol monster] ++ " hits " ++ [symbol player]  ++ " for " ++ show playerDamage  ++ " damage!"),
   (Console.Red, [symbol player]  ++ " hits " ++ [symbol monster] ++ " for " ++ show monsterDamage ++ " damage!")]

pickUpMessage thing = [(Console.Yellow, ("You pick up the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

nothingToPickUpMessage = [(Console.Yellow, "Nothing to pick up here!")]

dropMessage thing = [(Console.Yellow, ("You drop the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

cannotDropMessage thing = [(Console.Yellow, ("No space here to drop the " ++ (itemName thing) ++ ", " ++ [symbol thing]))]

nothingToDropMessage = [(Console.Yellow, "You don't have anything to drop!")]

victoryMessages = [(Console.Yellow, "You kick the chest, and it springs open with a *clang*, revealing a vast hoard of treasure!"),
                   (Console.Yellow, ""),
                   (Console.Red, "G A M E     O V E R")]

chestClosedMessage = [(Console.Cyan, "The chest makes a dull *clunk* when you kick it, but refuses to open.")]

changeLevelMessage newName = [(Console.Magenta, ("You climb the ladder to " ++ newName ++ "!"))]

{- Lab 5 messages below -}

equipMessage item slot = [(Console.Yellow, "You equip the " ++ (itemName item) ++ " in the " ++ slot ++ " slot.")]

unequipMessage item slot = [(Console.Yellow, "You unequip the " ++ (itemName item) ++ " from the " ++ slot ++ " slot.")]

slotFullMessage slot = [(Console.Yellow, "The " ++ slot ++ " slot already has something in it!")]

slotEmptyMessage slot = [(Console.Yellow, "The " ++ slot ++ " slot is empty!")]

noSuchItemMessage itemname = [(Console.Yellow, "Cannot equip nonexistent item " ++ itemname ++ "!")]

noSuchSlotMessage slotname = [(Console.Yellow, "No slot with name " ++ slotname ++ ".")]
