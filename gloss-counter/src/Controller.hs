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
       return $ gstate {infoToShow = ShowANumber newNumber}
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey
 :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate | c == KeySpace = shootBullet gstate
-- ShootBullet
                                                | c == KeyLeft = gstate { player = fmap (turnPlayer DirLeft) (player gstate)}
-- TurnPlayer DirLeft
                                                | c == KeyUp = gstate { player = fmap acceleratePlayer (player gstate)}
-- AcceleratePlayer
                                                | c == KeyRight = gstate { player = fmap (turnPlayer DirRight) (player gstate)}  
-- TurnPlayer DirRight                 
inputKey _ gstate = gstate -- Otherwise keep the same

turnPlayer :: Direction -> Player -> Player
turnPlayer DirLeft player = player { playerRotation = Rotation (angle - 1) }
                                    where Rotation angle = playerRotation player
turnPlayer DirRight player = player { playerRotation = Rotation (angle + 1) }
                                    where Rotation angle = playerRotation player

shootBullet :: GameState -> GameState
shootBullet g = g
-- Add Projectile to gstate with Players rotation and position, with a set speed. Check for 4 bullets to stop shooting.

acceleratePlayer :: Player -> Player
acceleratePlayer p = p { playerSpeed = newSpeed } 
                        where newSpeed = playerSpeed p
-- Add a fixed amount to the player speed. Decelerate when the player is not holding up. 