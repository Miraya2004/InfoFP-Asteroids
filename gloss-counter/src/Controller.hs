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
        (removeExpiredProjectiles
            (fmapProjectilesLifetime secs
                (applyProjectileMovement secs
                    (applyEnemyMovement secs
                        (applyMovement secs
                            (applyDeceleration secs
                                (useHeldKeys secs gstate))))))))
        { elapsedTime = elapsedTime gstate + secs }

-- Apply everything in step

applyDeceleration :: Float -> GameState -> GameState
applyDeceleration secs gstate
    | SpecialKey KeyUp `elem` heldKeys gstate = gstate
    | otherwise = gstate { player = fmap (deceleratePlayer secs) (player gstate) }

applyMovement :: Float -> GameState -> GameState
applyMovement secs gstate = gstate { player = fmap (movePlayer secs) (player gstate) }

applyProjectileMovement :: Float -> GameState -> GameState
applyProjectileMovement secs gstate = gstate { projectiles = map (moveProjectile secs) (projectiles gstate) }

updateProjectileLifetime :: Float -> Projectile -> Projectile 
updateProjectileLifetime secs projectile = projectile { lifetime = lifetime projectile - secs}

removeExpiredProjectiles :: GameState -> GameState
removeExpiredProjectiles gstate = gstate { projectiles = filter ((> 0) . lifetime) (projectiles gstate)}

applyEnemyMovement :: Float -> GameState -> GameState
applyEnemyMovement secs gstate = gstate { enemies = map moveEnemy (enemies gstate)}
    where
        moveEnemy (Aster asteroid) = Aster (moveAsteroid secs asteroid)
        moveEnemy enemy = enemy -- PlaceHolder until saucers are implemented


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


-- Projectile Functions
shootBullet :: GameState -> GameState
shootBullet gstate | length playerBullets >= 4 = gstate
                   | otherwise = 
                        case player gstate of
                            Nothing -> gstate
                            Just p  -> gstate { projectiles = createPlayerProjectile p : projectiles gstate}
                    where 
                        playerBullets = filter (not . enemy) (projectiles gstate)



createPlayerProjectile :: Player -> Projectile
createPlayerProjectile p = Proj{ enemy = False, projPosition = playerPosition p , projSpeed = bulletSpeed *^ direction, lifetime = bulletLifetime}
        where
            Rotation angle = playerRotation p 
            direction = vector2 (cos angle) (sin angle)
            bulletSpeed = 400
            bulletLifetime = 2.0

createSmartProjectile :: Position -> Position -> Projectile
createSmartProjectile enemyPosition playerPosition = Proj { enemy = True, projPosition = enemyPosition, projSpeed = bulletSpeed *^ direction, lifetime = bulletLifetime}
    where 
        dx = firstPos playerPosition - firstPos enemyPosition
        dy = secondPos playerPosition - secondPos enemyPosition
        distance = sqrt (dx * dx + dy * dy)
        direction = vector2 (dx / distance) (dy / distance)
        bulletSpeed = 400
        bulletLifetime = 2.0

createRandomProjectile :: Position -> StdGen -> (Projectile, StdGen)
createRandomProjectile enemyPosition gen = 
    let (angle, newGen) = randomR (0, 2 * pi) gen
        direction = vector2 (cos angle) (sin angle)
        bulletSpeed = 400
        bulletLifetime = 2.0
        projectile = Proj{ enemy = True, projPosition = enemyPosition, projSpeed = bulletSpeed *^ direction, lifetime = bulletLifetime }
    in (projectile, newGen)

moveProjectile :: Float -> Projectile -> Projectile
moveProjectile secs projectile =
    projectile { projPosition = newPosition }
    where
        Position (x, y) = projPosition projectile
        (vx, vy) = vector2XY (projSpeed projectile)

        position = Position (x + vx * secs, y + vy * secs)
        newPosition = wrapPosition position



--Player Movmment

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
                wrapX x | x < 0 = x + 800
                        | x > 800 = x - 800
                        | otherwise = x
                wrapY y | y < 0 = y + 600
                        | y > 600 = y - 600
                        | otherwise = y


-- Collisions functions 

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
--playerProjectileHit (Sauce(saucer)) projectile = saucerCollider (projPosition projectile) saucer

playerEnemyHit :: Player -> Enemy -> Bool
playerEnemyHit player (Aster(asteroid)) = asteroidCollider (playerPosition player) asteroid
--playerEnemyHit player (Sauce(saucer)) = saucerCollider (playerPosition player) asteroid


--Player Hits Functions

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

-- Asteroid Functions

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid secs asteroid =
    asteroid { asteroidPosition = newPosition }
    where
        Position (x, y) = asteroidPosition asteroid
        (vx, vy) = vector2XY (asteroidSpeed asteroid)

        position = Position (x + vx * secs, y + vy * secs)
        newPosition = wrapPosition position

spawnInitAsteroid :: StdGen -> (Asteroid, StdGen)
spawnInitAsteroid gen = 
    let minCoord = -1 * maxCoord
        maxCoord = if side < 2 then 300 else 400
        
        (side, gen1) = randomR (0, 3 :: Int ) gen
        (coordinate, gen2) = randomR (minCoord, maxCoord) gen1
        (angle, gen3) = randomR (0, 2*pi) gen2
        (speed, gen4) = randomR (20, 40) gen3

        position = case side of 
            0 -> Position(-400, coordinate)
            1 -> Position(400, coordinate)
            2 -> Position(coordinate, 300)
            3 -> Position(coordinate, -300)
        direction = vector2 (cos angle) (sin angle)

        asteroid = Asteroid{asteroidPosition = position, asteroidSize = 3 , asteroidSpeed = speed *^ direction}
    in (asteroid, gen4)

splitAsteroid :: Asteroid -> [Asteroid]
splitAsteroid asteroid | asteroidSize asteroid <= 1 = []
                       | otherwise = 
                            [createSplit (-pi / 4), createSplit (pi / 4)]
                        where 
                            (vx, vy) = vector2XY (asteroidSpeed asteroid)

                            createSplit angle = 
                                Asteroid 
                                    { asteroidPosition = asteroidPosition asteroid
                                    , asteroidSpeed = vector2 (vx * cos angle - vy * sin angle) (vx * sin angle + vy * cos angle)
                                    , asteroidSize = asteroidSize asteroid -1 }

checkAsteroidCollisions :: GameState -> GameState
checkAsteroidCollisions gstate = 
    let newState = gstate { enemies = concatMap updateEnemy (enemies gstate)
                          , projectiles = remainingProjectiles}
    in if playerWasHit 
        then destroyPlayer newState
        else newState
    where                     
        updateEnemy (Aster asteroid)
            | projectileHit asteroid = map Aster (splitAsteroid asteroid)
            | saucerHit asteroid = []
            | playerHit asteroid = []
            | otherwise = [Aster asteroid]
        
        updateEnemy enemy@(Sauce _) = [enemy]
        
        projectileHit asteroid = any (\projectile -> playerProjectileHit (Aster asteroid) projectile) (projectiles gstate)
        saucerHit asteroid = any (\enemy -> case enemy of 
                                            Sauce saucer -> asteroidCollider (saucserPosition saucer) asteroid
                                            _ -> False) (enemies gstate)
        playerHit asteroid =
            case player gstate of 
                Nothing -> False 
                Just p -> playerEnemyHit p (Aster asteroid)
        playerWasHit =
            case player gstate of
                Nothing -> False
                Just p -> any (playerEnemyHit p) asteroidEnemies
        
        asteroidEnemies = [Aster asteroid | Aster asteroid <- enemies gstate]
        
        remainingProjectiles = filter (not . projectileHitAsteroid) (projectiles gstate)

        projectileHitAsteroid projectile = any (\enemy -> case enemy of
                                                    Aster asteroid -> playerProjectileHit (Aster asteroid) projectile
                                                    _ -> False) (enemies gstate)


--Helper functions

firstPos :: Position -> Float
firstPos (Position(x, y)) = x
secondPos :: Position -> Float
secondPos (Position(x, y)) = y

fmapProjectilesLifetime :: Float -> GameState -> GameState
fmapProjectilesLifetime secs gstate = gstate { projectiles = map (updateProjectileLifetime secs) (projectiles gstate) }

                                    









-- TODO:
-- Player:
--   Add screen wrapping. [x]
--   Add player lives and respawning. [x] (TODO respawnInvuln)
--   Add player destruction. [x]

-- Projectiles:
--   Implement bullet creation. [x]
--   Add projectile movement. [x]
--   Add projectile lifetime. [x]
--   Add firing cooldown and bullet limit. [x]

-- Asteroids:
--   Implement asteroid data structures and movement. [x]
--   Add asteroid spawning. [x]
--   Add asteroid screen wrapping. [x] 
--   Implement asteroid destruction and splitting. [x]

-- Enemies:
--   Implement enemy saucers.
--   Add saucer movement and behaviour.
--   Add saucer spawning.

-- Game mechanics:
--   Implement collision detection. [o]
--   Implement collision responses. [o]
--   Add scoring.
--   Add difficulty progression.

-- Game flow:
--   Add game over state.
--   Add restart functionality.
