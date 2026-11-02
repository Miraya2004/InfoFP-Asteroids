-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.VectorSpace
import Data.Vector2

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | screenState gstate == Paused = return gstate
                 | otherwise = return $ (useHeldKeys gstate) { elapsedTime = elapsedTime gstate + secs }

--  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--  = -- We show a new random number
--    do randomNumber <- randomIO
--       let newNumber = abs randomNumber `mod` 10
--       return $ (useHeldKeys gstate) {infoToShow = ShowANumber newNumber}
--  | otherwise
-- Just update the elapsed time
    
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 'p' || c == 'P' = gstate { screenState = if screenState gstate == Paused then Playing else Paused}

inputKey (EventKey key Down _ _) gstate | key `elem` heldKeys gstate = gstate
                                        | otherwise = gstate { heldKeys = key : heldKeys gstate}

inputKey (EventKey key Up _ _) gstate = gstate { heldKeys = filter (/= key) (heldKeys gstate) }

inputKey _ gstate = gstate -- Otherwise keep the same

useHeldKeys :: GameState -> GameState
useHeldKeys gstate = foldl useKey gstate (heldKeys gstate)

useKey :: GameState -> Key -> GameState
useKey gstate (SpecialKey KeySpace) = shootBullet gstate
useKey gstate (SpecialKey KeyLeft) = gstate { player = fmap (turnPlayer DirLeft) (player gstate) }
useKey gstate (SpecialKey KeyUp) = gstate { player = fmap acceleratePlayer (player gstate)}
useKey gstate (SpecialKey KeyRight) = gstate { player = fmap (turnPlayer DirRight) (player gstate)}

useKey gstate _ = gstate

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
                    where 
                        acceleration = 5
                        topSpeed = 300
                        Rotation playerAngle = playerRotation p 

                        accelerationVector = vector2 (cos playerAngle) (sin playerAngle)

                        acceleratedSpeed = playerSpeed p ^+^ (acceleration *^ accelerationVector)

                        currentSpeed = norm acceleratedSpeed
                        
                        newSpeed | currentSpeed > topSpeed = (topSpeed / currentSpeed) *^ acceleratedSpeed 
                                 | otherwise = acceleratedSpeed
-- Add a fixed amount to the player speed. Decelerate when the player is not holding up. 