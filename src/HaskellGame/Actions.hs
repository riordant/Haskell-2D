module HaskellGame.Actions where

import Prelude ( Num(..), Eq(..),
                 Char(), Maybe(..), String(),
                 ($), (++), (.), fst, snd, not, return )

import qualified Data.List as List
import Data.List (filter, find, any, null, head, (!!))

import qualified Data.Maybe as Maybe
import Data.Maybe

import HaskellGame.Datatypes
import HaskellGame.GameMonad
import HaskellGame.Messages
import HaskellGame.Battle
import HaskellGame.Collisions
import HaskellGame.Utils.Dictionary

movePlayer :: Char -> GameAction ()
movePlayer keyPressed = do
  p <- get player
  let (x, y) = position p
  let newPosition = case keyPressed of
                      'i' -> (x, (y-1))
                      'j' -> ((x-1), y)
                      'k' -> (x, (y+1))
                      'l' -> ((x+1), y)
                      _   -> (x, y)
  isCollision <- detectCollision newPosition

  if (fst isCollision) then
    change (\s -> s { collided = snd isCollision })
  else
    let newPlayer = p { pos = newPosition }
    in change (\s -> s { player = newPlayer, collided = Nothing } )

doAttack :: GameAction ()
doAttack = do
  thePlayer <- get player
  theMap    <- get currentLevel
  msgs      <- get messages

  let allMonsters = monsters theMap
  let nearbyMonsters = filter ((==1) . (distance thePlayer)) allMonsters

  if not (null nearbyMonsters) then
    attackMonsters nearbyMonsters
  else
    change (\s -> s { messages = msgs ++ missedMessage })

attackMonsters :: [Monster] -> GameAction ()
attackMonsters [] = return ()
attackMonsters (firstMonster:rest) = do
  oldPlayer   <- get player
  oldLevel    <- get currentLevel
  oldMessages <- get messages

  let (newPlayer, newMonster) = fight (oldPlayer, firstMonster)
  let (damageP, damageM) = ((health oldPlayer - health newPlayer), (health firstMonster - health newMonster))
  let battleMessages = hitMessage newMonster damageM newPlayer damageP
  let oldMonsters = monsters oldLevel
  let newMonsters = (newMonster:(List.delete firstMonster oldMonsters))
  let newMessages = oldMessages ++ battleMessages
  let newLevel = oldLevel { monsters = newMonsters }

  change (\s -> s { player = newPlayer, currentLevel = newLevel, messages = newMessages })
  attackMonsters rest


pickUpItem :: GameAction ()
pickUpItem = do
  plyr  <- get player
  level <- get currentLevel
  msgs  <- get messages

  let droppedItems = filter isDroppedItem (objects level)
  let couldPickUp = find ((==(position plyr)) . position) droppedItems

  case couldPickUp of
    Nothing ->
      change (\s -> s { messages = msgs ++ nothingToPickUpMessage })

    (Just ob@(Dropped i _)) ->
      let newPlayer = plyr { inventory = insert (itemName i) i (inventory plyr) }
          newLevel = level { objects = List.delete ob (objects level) }
          newMessages = msgs ++ pickUpMessage i
      in change (\s -> s { player = newPlayer, currentLevel = newLevel, messages = newMessages })

dropItem :: GameAction ()
dropItem = do
  plyr  <- get player
  level <- get currentLevel
  msgs  <- get messages

  let couldDrop = if empty (inventory plyr) then Nothing else Just (head (toList (inventory plyr)))
  let droppedItems = filter isDroppedItem (objects level)

  case couldDrop of
    Nothing ->
      change (\s -> s { messages = msgs ++ nothingToDropMessage } )

    (Just i@(theName, theItem)) -> do
      if any ((==(position plyr)) . position) droppedItems then
        change (\s -> s { messages = msgs ++ cannotDropMessage theItem } )
      else
        let newPlayer = plyr { inventory = delete theName (inventory plyr) }
            newObjects = (Dropped theItem (position plyr)):(objects level)
            newLevel = level { objects = newObjects }
            newMessages = msgs ++ dropMessage theItem
        in
          change (\s -> s { currentLevel = newLevel, player = newPlayer, messages = newMessages } )

{- Lab 5: implement this function -}

doCommand :: Command -> GameAction ()
doCommand cmd = do
  plyr  <- get player
  level <- get currentLevel
  msgs  <- get messages
  
  case cmd of
   (Equip itemK slotK) -> 
     let 
         invent = inventory plyr
         allSlots = slots plyr
         slotList = toList allSlots
         itemList = toList invent
         itemL = filter((==itemK).fst) itemList       
     in if not (null itemL)
        then
        let 
            slotL = filter((==slotK).fst) slotList {-pulls out the slot tuple if it exists. -}
            item = snd(itemL !! 0)
        in if not (null slotL) {- this checks if the key is in the dict. -}
           then
           let slot = (slotL !! 0)
           in if (snd slot == Nothing)
              then
              let 
                  newInvent = delete (itemName item) (invent)
                  newSlots = insert (fst slot) (Just item) (allSlots)
                  newPlayer = plyr { inventory = newInvent, slots = newSlots } 
                  newMsgs = msgs ++ equipMessage item slotK
              in 
                 change ( \s -> s { player = newPlayer, messages = newMsgs } )
           else 
                change ( \s -> s { messages = msgs ++ slotFullMessage slotK } )  
         else 
             change ( \s -> s { messages = msgs ++ noSuchSlotMessage slotK } )
      else 
           change ( \s -> s { messages = msgs ++ noSuchItemMessage itemK } )
             
          
   (Unequip slotK) ->
      let 
        invent = inventory plyr
        allSlots = slots plyr
        slotList = toList allSlots
        slotL = filter((==slotK).fst) slotList {-pulls out the slot tuple if it exists. -}
      in if not(null slotL) {- this checks if the key is in the dict. -}
         then
         let slot = (slotL !! 0)
         in if (snd slot /= Nothing)
            then
            let
                item = fromJust (snd slot)
                newSlots = insert (fst slot) Nothing allSlots
                newInvent = insert (itemName item) (item) (invent)
                newPlayer = plyr { inventory = newInvent, slots = newSlots } 
                newMsgs = msgs ++ unequipMessage item slotK
            in
               change ( \s -> s { player = newPlayer, messages = newMsgs } )
         else 
              change ( \s -> s { messages = msgs ++ slotEmptyMessage slotK } )
      else 
           change ( \s -> s { messages = msgs ++ noSuchSlotMessage slotK } )
           
   (None) -> return()
      
     
           


           
    

          
        
      
     
     
     
     
     
     
     
