{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude (
                 Num(..), Ord(..), Fractional(..), Eq(..), Read(..),
                 Either(..), Bool(..), Maybe(..), String(), Integer(), IO(), Char(), Int(),
                 ($), round, reads
               )

import Data.IORef (IORef(),
                   newIORef, readIORef, writeIORef, modifyIORef)

import System.IO (Handle(), BufferMode(..),
                  stdin, stdout, hReady, hPutStr, hGetChar, hPutChar, hFlush, hSetBuffering, hSetEcho, hGetLine)

import Data.List ((++), head, tail, last, length)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, when, return, zipWithM_)
import System.Exit (exitFailure, exitSuccess)
import qualified System.Console.ANSI as Console

import HaskellGame.Datatypes
import HaskellGame.Utils
import HaskellGame.Utils.Dictionary
import HaskellGame.Rendering
import HaskellGame.Interaction
import HaskellGame.Battle
import HaskellGame.Engine

{- Aliases to make it clearer what's happening in main -}

-- x and y coords to start messages at
messageX = 20
messageY = 3

-- x and y coords of the status bar
statusX = 20
statusY = 1

-- x and y coords of the controls message
controlsX = 20
controlsY = 12

-- x and y coords of the game time display
timeX = 70
timeY = 1

-- x and y coords of the command box area
cmdBoxX = 20
cmdBoxY = 14

{- Make the initial game world -}
initialScene = Scene level1 [level2] thePlayer
                     [(Console.Cyan, "WELCOME TO HASKELLGAME")]
                     Nothing
  where
    -- First level

    level1 = Level (createMap "Level 1" 20 11 theTiles) theObjects theMonsters

    theTiles =
      "                    " ++
      "   ###########      " ++
      "  #...........#     " ++
      "  #............#    " ++
      "  #.............#   " ++
      "  #.............#   " ++
      "  #.............#   " ++
      "   #............#   " ++
      "    #...........#   " ++
      "     ###########    " ++
      "                    "

    theObjects = [ Chest (13,6),
                   Ladder (6,3) (Destination "Level 2" (9,12)) ]

    theMonsters = [Zombie 10 1 (6,4),
                   Dragon 20 2 (8,4)]

    -- Second level

    level2 = Level (createMap "Level 2" 20 14 theTiles2) theObjects2 theMonsters2

    theTiles2 =
      "                    " ++
      "   ####   ####      " ++
      "  #....###....#     " ++
      "  #............#    " ++
      " #..............#   " ++
      " #..............#   " ++
      "  #.............#   " ++
      "   ##........#..#   " ++
      "    ##.....## ##    " ++
      "     #.....#        " ++
      "     #.....#        " ++
      "     ###...#        " ++
      "       #...#        " ++
      "        ###         "

    theObjects2 = [Chest (13,6),
                   Ladder (9,12) (Destination "Level 1" (6,3)),
                   Dropped theSword (5,5),
                   Dropped theArmour (6,6) ]

    theSword = Item "Sword" "A pointy sword" '/' (fromList [("Damage", 10)]) Weapon

    theArmour = Item "Chainmail" "Some tough chain mail" '@' (fromList [("Toughness", 5)]) Armour

    theMonsters2 = [Dragon 10 2 (6,8),
                    Dragon 20 2 (9,4)]

    thePlayer = Player 10                     -- Initial HP
                       0                      -- Initial XP
                       [ ("Strength", 5),     -- Initial stats
                         ("Toughness", 1) ]
                       [ ("Fisticuffs", 1) ]  -- Initial skills
                       (10,5)                 -- Initial location
                       (fromList [])
                       (fromList [("Left Hand",  Nothing),  -- The player's equipment slots
                                  ("Right Hand", Nothing),
                                  ("Torso",      Nothing),
                                  ("Legs",       Nothing)])

{- Make the initial engine state -}
engineState = EngineState {
                screen = stdout,
                keyboard = stdin,
                frameRate = 18,
                messageLimit = 7,
                frameNumber = 0,
                keyPressed = ' ',
                command = None,
                isGameOver = False,
                scene = initialScene
              }

main = do
  hSetBuffering (keyboard engineState) NoBuffering
  hSetBuffering (screen engineState) LineBuffering
  hSetEcho (screen engineState) False
  Console.hHideCursor (screen engineState)
  hFlush (screen engineState)
  gameState <- newIORef $ engineState
  forever $ do
    currentState <- readIORef gameState
    -- Read the game state

    keypress <- getCharacter (keyboard currentState)
    -- get the key press from the player

    cmd <- if keypress == '`' then do
             typedString <- readCmd (screen engineState) (keyboard engineState)
             case reads typedString of
              [(x, "")] -> return x
              _         -> return None
           else
             return None
    -- if the player pressed ` then read a whole line from them

    let newState = runGame (currentState {keyPressed = keypress, command = cmd})
    -- run one time step of the game, and get the new game state

    Console.hClearScreen (screen currentState)
    -- Clear the screen

    renderScene (screen newState) (scene newState)
    -- render the new scene to the screen

    showStatus (screen newState) (statusX, statusY) (player (scene newState))
    -- show the status bar

    showTimeElapsed (screen newState) (timeX, timeY) newState
    -- show the time display

    showControls (screen newState) (controlsX, controlsY)
    -- show the game controls legend

    showMessages newState
    -- show the messages in sequence underneath each other

    hFlush (screen newState)
    -- update the screen

    unless (isGameOver newState) $ do
      -- unless the game is over
      writeIORef gameState newState
      threadDelay $ round $ 1.0e6/(fromInteger $ frameRate newState)
      -- sleep for one frame period

    when (isGameOver newState) $ do
      -- when the game is over, show the end screen and quit
      threadDelay $ round 2.3e6
      Console.hSetSGR (screen newState) [Console.Reset]
      Console.hSetCursorPosition (screen newState) 0 0
      Console.hShowCursor (screen newState)
      Console.hClearScreen (screen newState)
      hFlush (screen newState)
      gobbleInput (keyboard newState)
      exitSuccess
  where
    gobbleInput :: Handle -> IO ()
    gobbleInput handle = do
      r <- hReady handle
      when r $ do
        hGetChar handle
        gobbleInput handle
      return ()

    getCharacter :: Handle -> IO Char
    getCharacter handle = do
      r <- hReady handle
      if r then
        hGetChar handle
      else return ' '

    showMessages :: EngineState -> IO ()
    showMessages theState = do
      zipWithM_ (printAt (screen theState))
                [(messageX, y) | y <- [messageY..]]
                (messages (scene theState))

    readCmd s k = do
      Console.hSetCursorPosition s cmdBoxY cmdBoxX
      Console.hShowCursor s
      hSetEcho s True
      hFlush s
      line <- hGetLine k
      Console.hHideCursor s
      hSetEcho s False
      hFlush s
      return line



