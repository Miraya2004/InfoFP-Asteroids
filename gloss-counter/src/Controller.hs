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
step secs gstate
    | screenState gstate == Paused = return gstate
    | otherwise = return $ 
                    (checkPlayerCollisions
                        (applyMovement secs 
                            (applyDeceleration secs 
                                (useHeldKeys secs gstate))))
        { elapsedTime = elapsedTime gstate + secs }

applyDeceleration :: Float -> GameState -> GameState
applyDeceleration secs gstate
    | SpecialKey KeyUp `elem` heldKeys gstate = gstate
    | otherwise = gstate { player = fmap (deceleratePlayer secs) (player gstate) }

applyMovement :: Float -> GameState -> GameState
applyMovement secs gstate = gstate { player = fmap (movePlayer secs) (player gstate) }


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

useHeldKeys :: Float -> GameState -> GameState
useHeldKeys secs gstate = foldl (useKey secs) gstate (heldKeys gstate)

useKey :: Float -> GameState -> Key -> GameState
useKey secs gstate (SpecialKey KeySpace) = shootBullet gstate
useKey secs gstate (SpecialKey KeyLeft) = gstate { player = fmap (turnPlayer secs DirLeft) (player gstate) }
useKey secs gstate (SpecialKey KeyUp) = gstate { player = fmap (acceleratePlayer secs) (player gstate)}
useKey secs gstate (SpecialKey KeyRight) = gstate { player = fmap (turnPlayer secs DirRight) (player gstate)}

useKey secs gstate _ = gstate

turnPlayer :: Float -> Direction -> Player -> Player
turnPlayer secs direction player = player { playerRotation = Rotation (resAngle) }
                                    where 
                                        Rotation angle = playerRotation player
                                        rotationSpeed = 2 * pi
                                        directionSign = case direction of
                                            DirLeft -> -1
                                            DirRight -> 1
                                        newAngle = (angle + directionSign * rotationSpeed * secs) 
                                        resAngle | newAngle > (2 * pi) = newAngle - (2 * pi)
                                                 | newAngle < 0 = newAngle + (2 * pi)
                                                 | otherwise = newAngle

shootBullet :: GameState -> GameState
shootBullet g = g
-- Add Projectile to gstate with Players rotation and position, with a set speed. Check for 4 bullets to stop shooting.


acceleratePlayer :: Float -> Player -> Player
acceleratePlayer secs p = p { playerSpeed = newSpeed } 
                    where 
                        acceleration = 40
                        topSpeed = 300
                        Rotation playerAngle = playerRotation p 

                        accelerationVector = vector2 (cos playerAngle) (sin playerAngle)

                        acceleratedSpeed = playerSpeed p ^+^ ((acceleration * secs) *^ accelerationVector)

                        currentSpeed = norm acceleratedSpeed
                        
                        newSpeed | currentSpeed > topSpeed = (topSpeed / currentSpeed) *^ acceleratedSpeed 
                                 | otherwise = acceleratedSpeed
-- Add a fixed amount to the player speed. Decelerate when the player is not holding up. 
deceleratePlayer :: Float -> Player -> Player 
deceleratePlayer secs p = p { playerSpeed = newSpeed }
                    where
                        friction = 0.8
                        factor = max 0 (1 - friction * secs)
                        spd = playerSpeed p
                        newSpeed | vector2Rho (playerSpeed p) > 1 = factor *^ playerSpeed p 
                                 | otherwise = 0 *^ playerSpeed p
                    
movePlayer :: Float -> Player -> Player
movePlayer secs p = p { playerPosition = newPosition }
    where
        Position (x, y) = playerPosition p
        (vx, vy) = vector2XY (playerSpeed p)
        position = Position (x + vx * secs, y + vy * secs)
        newPosition = wrapPosition position

wrapPosition :: Position -> Position
wrapPosition position = Position (newX, newY)
            where 
                newX = wrapX (firstPos position)
                newY = wrapY (secondPos position) 
                wrapX x | x < -400 = x + 800
                        | x > 400 = x - 800
                        | otherwise = x
                wrapY y | y < -300 = y + 600
                        | y > 300 = y - 600
                        | otherwise = y

asteroidCollider :: Position -> Asteroid -> Bool
asteroidCollider pos asteroid | asteroidSize asteroid == 3 = -30 < x && x < 30 && -30 < y && y < 30
                              | asteroidSize asteroid == 2 = -20 < x && x < 20 && -20 < y && y < 20
                              | asteroidSize asteroid == 1 = -10 < x && x < 10 && -10 < y && y < 10
                              | otherwise = False
                            where 
                              x = firstPos (asteroidPosition asteroid) - firstPos pos
                              y = secondPos (asteroidPosition asteroid) - secondPos pos
                            

enemyProjectileHit :: Player -> Projectile -> Bool
enemyProjectileHit player projectile = -5 < x && x < 5 && -5 < y && y < 5
                                where 
                                    x = firstPos (playerPosition player) - firstPos (projPosition projectile)
                                    y = secondPos (playerPosition player) - secondPos (projPosition projectile)

playerProjectileHit :: Enemy -> Projectile -> Bool
playerProjectileHit (Aster(asteroid)) projectile = asteroidCollider (projPosition projectile) asteroid

playerEnemyHit :: Player -> Enemy -> Bool
playerEnemyHit player (Aster(asteroid)) = asteroidCollider (playerPosition player) asteroid

respawnPlayer :: Player -> Player
respawnPlayer oldPlayer = initialPlayer { lives = lives oldPlayer }

destroyPlayer :: GameState -> GameState
destroyPlayer gstate = case player gstate of
                        Nothing -> gstate 
                        Just p -> 
                            let remLives = lives p - 1
                            in if remLives > 0 
                                then gstate {player = Just (respawnPlayer (p {lives = remLives}))}
                                else gstate {player = Nothing}

checkPlayerCollisions :: GameState -> GameState
checkPlayerCollisions gstate = 
    case player gstate of 
        Nothing -> gstate
        Just p -> 
            if asteroidCollision p || projectileCollision p 
                then destroyPlayer gstate
                else gstate
    where 
        asteroidCollision p = any (playerEnemyHit p) [e | e@(Aster _) <- enemies gstate]
        projectileCollision p = any (enemyProjectileHit p) (filter enemy (projectiles gstate))









--Helper functions

firstPos :: Position -> Float
firstPos (Position(x, y)) = x
secondPos :: Position -> Float
secondPos (Position(x, y)) = y


                                    









-- TODO:
-- Player:
--   Add screen wrapping. [x]
--   Add player lives and respawning. [x] (TODO respawnInvuln)
--   Add player destruction. [x]

-- Projectiles:
--   Implement bullet creation.
--   Add projectile movement.
--   Add projectile lifetime.
--   Add firing cooldown and bullet limit.

-- Asteroids:
--   Implement asteroid data structures and movement.
--   Add asteroid spawning.
--   Add asteroid screen wrapping.
--   Implement asteroid destruction and splitting.

-- Enemies:
--   Implement enemy saucers.
--   Add saucer movement and behaviour.
--   Add saucer spawning.

-- Game mechanics:
--   Implement collision detection. [O]
--   Implement collision responses.
--   Add scoring.
--   Add difficulty progression.

-- Game flow:
--   Add game over state.
--   Add restart functionality.
