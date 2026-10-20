-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0 Nothing Nothing Nothing 0 Nothing
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate | c == KeySpace = gstate { infoToShow = ShowAChar 's'}
                                                | c == KeyLeft = gstate { infoToShow = ShowAChar 'l'}
                                                | c == KeyUp = gstate { infoToShow = ShowAChar 'u'}
                                                | c == KeyRight = gstate { infoToShow = ShowAChar 'r'}                   
inputKey _ gstate = gstate -- Otherwise keep the same