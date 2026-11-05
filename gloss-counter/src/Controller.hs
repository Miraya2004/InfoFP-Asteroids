-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.VectorSpace
import Data.Vector2
import Data.List (mapAccumL)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    | screenState gstate == Paused = return gstate
    | otherwise = return $
        (checkEnemyCollisions
            (checkPlayerCollisions
                (removeExpiredProjectiles
                    (fmapProjectilesLifetime secs
                        (applySaucerShooting secs
                            (applyProjectileMovement secs
                                (removeSaucers
                                    (applyEnemyMovement secs
                                        (applySaucerDirections secs
                                            (applyMovement secs
                                                (applyDeceleration secs
                                                    (useHeldKeys secs
                                                        (periodicSpawner secs
                                                            (spawnAsteroids gstate))))))))))))))
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
        moveEnemy (Sauce saucer) = Sauce (moveSaucer secs saucer)

applySaucerShooting :: Float -> GameState -> GameState
applySaucerShooting secs gstate =
    let 
        (newGen, results) = mapAccumL shootEnemy (randomGenerator gstate) (enemies gstate)
        newEnemies = map fst results
        newProjectiles = [p | (_, Just p) <- results]
    in 
        gstate { enemies = newEnemies, projectiles = newProjectiles ++ projectiles gstate, randomGenerator = newGen}
    where
        shootEnemy gen (Aster asteroid) = (gen, (Aster asteroid, Nothing))
        shootEnemy gen (Sauce saucer) = 
            let 
                (newSaucer, projectile, newGen) = updateSaucerFiring secs (player gstate) saucer gen
            in  
                (newGen, (Sauce newSaucer, projectile))

applySaucerDirections :: Float -> GameState -> GameState
applySaucerDirections secs gstate =
    let 
        (newGen, newEnemies) = mapAccumL changeEnemyDir (randomGenerator gstate) (enemies gstate)
        changeEnemyDir gen (Aster asteroid) = (gen, Aster asteroid)
        changeEnemyDir gen (Sauce saucer)   = 
            let (newSaucer, newGen) = changeSaucerDirection secs saucer gen
            in  (newGen, Sauce newSaucer)
    in
        gstate { enemies = newEnemies, randomGenerator = newGen}

spawnAsteroids :: GameState -> GameState
spawnAsteroids gstate | null (enemies gstate) = 
                        let 
                            (asteroid1, gen1) = spawnInitAsteroid (randomGenerator gstate)
                            (asteroid2, gen2) = spawnInitAsteroid gen1
                            (asteroid3, gen3) = spawnInitAsteroid gen2
                            (asteroid4, gen4) = spawnInitAsteroid gen3
                        in  gstate {enemies = map Aster [asteroid1, asteroid2, asteroid3, asteroid4], randomGenerator = gen4}
                      | otherwise = gstate
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

wrapYPosition :: Position -> Position
wrapYPosition position = Position (firstPos position, newY)
            where
                newY = wrapY (secondPos position)
                wrapY y | y < 0 = y + 600
                        | y > 600 = y - 600
                        | otherwise = y

-- Collisions functions 

asteroidCollider :: Position -> Asteroid -> Bool
asteroidCollider pos asteroid | asteroidSize asteroid == 3 = withinBounds 30
                              | asteroidSize asteroid == 2 = withinBounds 20
                              | asteroidSize asteroid == 1 = withinBounds 10
                              | otherwise = False
                            where 
                              x = wrappedDifference 800 (firstPos (asteroidPosition asteroid)) (firstPos pos)
                              y = wrappedDifference 800 (secondPos (asteroidPosition asteroid)) (secondPos pos)
                              withinBounds size = -size < x && x < size && -size < y && y < size

saucerCollider :: Position -> Saucer -> Bool
saucerCollider pos saucer | saucerSize saucer == 1 = withinBounds 10
                          | saucerSize saucer == 2 = withinBounds 20
                          | otherwise = False
                          where
                              x = wrappedDifference 800 (firstPos (saucerPosition saucer)) (firstPos pos)
                              y = wrappedDifference 600 (secondPos (saucerPosition saucer)) (secondPos pos)
                              withinBounds size = -size < x && x < size && -size < y && y < size

enemyProjectileHit :: Player -> Projectile -> Bool
enemyProjectileHit player projectile = -5 < x && x < 5 && -5 < y && y < 5
                                where 
                                    x = wrappedDifference 800 (firstPos (playerPosition player)) (firstPos (projPosition projectile))
                                    y = wrappedDifference 600 (secondPos (playerPosition player)) (secondPos (projPosition projectile))

playerProjectileHit :: Enemy -> Projectile -> Bool
playerProjectileHit (Aster(asteroid)) projectile = asteroidCollider (projPosition projectile) asteroid
playerProjectileHit (Sauce(saucer)) projectile = saucerCollider (projPosition projectile) saucer

playerEnemyHit :: Player -> Enemy -> Bool
playerEnemyHit player (Aster(asteroid)) = asteroidCollider (playerPosition player) asteroid
playerEnemyHit player (Sauce(saucer)) = saucerCollider (playerPosition player) saucer


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
            if playerCollision p || projectileCollision p 
                then destroyPlayer gstate
                else gstate
    where 
        playerCollision p = any (playerEnemyHit p) (enemies gstate)
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

checkEnemyCollisions :: GameState -> GameState
checkEnemyCollisions gstate = 
    let newState = gstate { enemies = concatMap updateEnemy (enemies gstate)
                          , projectiles = remainingProjectiles}
    in if playerWasHit 
        then destroyPlayer newState
        else newState
    where                     
        updateEnemy (Aster asteroid)
            | projectileHitAsteroid asteroid = map Aster (splitAsteroid asteroid)
            | saucerHitAsteroid asteroid = map Aster (splitAsteroid asteroid)
            | playerHitAsteroid asteroid = map Aster (splitAsteroid asteroid)
            | otherwise = [Aster asteroid]
        
        updateEnemy (Sauce saucer) 
            | asteroidHitSaucer saucer = []
            | projectileHitSaucer saucer = []
            | playerHitSaucer saucer = []
            | otherwise = [Sauce saucer]
        --Asteroids
        projectileHitAsteroid asteroid = any (\projectile -> playerProjectileHit (Aster asteroid) projectile) (projectiles gstate)

        saucerHitAsteroid asteroid = any (\enemy -> case enemy of 
                                            Sauce saucer -> asteroidCollider (saucerPosition saucer) asteroid
                                            _ -> False) (enemies gstate)

        playerHitAsteroid asteroid =
            case player gstate of 
                Nothing -> False 
                Just p -> playerEnemyHit p (Aster asteroid)
        --Saucers
        asteroidHitSaucer saucer = any (\enemy -> case enemy of
                                        Aster asteroid -> asteroidCollider (saucerPosition saucer) asteroid 
                                        _ -> False ) (enemies gstate)

        projectileHitSaucer saucer = any (\projectile -> playerProjectileHit (Sauce saucer) projectile) (projectiles gstate)

        playerHitSaucer saucer = 
            case player gstate of
                Nothing -> False
                Just p -> playerEnemyHit p (Sauce saucer)

        playerWasHit =
            case player gstate of
                Nothing -> False
                Just p -> any (playerEnemyHit p) (enemies gstate)
        
        remainingProjectiles = filter (not . projectileHitEnemy) (projectiles gstate)

        projectileHitEnemy projectile = any (\enemy -> playerProjectileHit enemy projectile) (enemies gstate)


--Saucer functions
spawnSaucer :: StdGen -> (Saucer, StdGen)
spawnSaucer gen =
    let
        (side, gen1) = randomR (0, 1 :: Int) gen
        (randY, gen2) = randomR (-280, 280) gen1
        (size, gen3) = randomR (1, 2 :: Int) gen2
        (speed_, gen4) = randomR (30, 60) gen3
        cooldown = 1.5
        angle = 0

        direction = if side == 0 then 1 else -1
        x = if side == 0 then -400 else 400
        position = Position (x, randY)
        speed = vector2 (direction * speed_ * cos angle) 0

        saucer = Saucer{ saucerPosition = position
        , saucerSpeed = speed 
        , saucerSize = size
        , saucerShootingCooldown = cooldown
        }
    in 
        (saucer, gen4)

moveSaucer :: Float -> Saucer -> Saucer
moveSaucer secs saucer =
    saucer { saucerPosition = newPosition}
    where
        (vx, vy) = vector2XY (saucerSpeed saucer)
        Position (x, y) = saucerPosition saucer
        
        position = Position (x + vx * secs, y + vy * secs)
        newPosition = wrapYPosition position

updateSaucerFiring :: Float -> Maybe Player -> Saucer -> StdGen -> (Saucer, Maybe Projectile, StdGen)
updateSaucerFiring secs maybePlayer saucer gen 
    | newCooldown > 0 = (saucer { saucerShootingCooldown = newCooldown }, Nothing, gen)
    | saucerSize saucer == 1 =
        case maybePlayer of
            Nothing -> (saucer {saucerShootingCooldown = 1.5}, Nothing, gen)
            Just p ->
                let projectile = createSmartProjectile (saucerPosition saucer) (playerPosition p)
                in (saucer { saucerShootingCooldown = 1.5 }, Just projectile, gen)
    | saucerSize saucer == 2 =
        let (projectile, newGen) = createRandomProjectile (saucerPosition saucer) gen
        in (saucer {saucerShootingCooldown = 1.5}, Just projectile, newGen)
    | otherwise = (saucer, Nothing, gen)
    where newCooldown = saucerShootingCooldown saucer - secs

periodicSpawner :: Float -> GameState -> GameState
periodicSpawner secs gstate 
    | floor (newTime / spawnCycleTime) > floor (oldTime / spawnCycleTime) = 
        let 
            (saucer, newGen) = spawnSaucer (randomGenerator gstate)
        in 
            gstate { enemies = Sauce saucer : enemies gstate, randomGenerator = newGen}
    | otherwise = gstate
    where
        spawnCycleTime = 30
        oldTime = elapsedTime gstate
        newTime = oldTime + secs

changeSaucerDirection :: Float -> Saucer -> StdGen -> (Saucer, StdGen)
changeSaucerDirection secs saucer gen = 
    let
        (vx, vy) = vector2XY (saucerSpeed saucer)
        speed = sqrt (vx * vx + vy * vy)
        direction = if vx >= 0 then 1 else -1
        changeChance = 1 - (1 - 0.05) ** secs
        (roll, gen1) = randomR (0.0, 1.0) gen

        (angle, gen2) = if roll < changeChance then
                            let (index, g2) = randomR (0, 2 :: Int) gen1
                            in ([0, pi/6, -pi/6] !! index, g2)
                        else (atan2 vy (abs vx), gen1)
        newSpeed = vector2 (direction * speed * cos angle) (speed * sin angle)
    in (saucer {saucerSpeed = newSpeed}, gen2)

removeSaucers :: GameState -> GameState
removeSaucers gstate = gstate { enemies = filter keepEnemy (enemies gstate)}
    where
        keepEnemy (Aster _) = True
        keepEnemy (Sauce saucer) = 
            let 
                Position (x, _) = saucerPosition saucer
            in
                x <= 400 && x >= -400




--Helper functions

firstPos :: Position -> Float
firstPos (Position(x, y)) = x
secondPos :: Position -> Float
secondPos (Position(x, y)) = y

fmapProjectilesLifetime :: Float -> GameState -> GameState
fmapProjectilesLifetime secs gstate = gstate { projectiles = map (updateProjectileLifetime secs) (projectiles gstate) }

wrappedDifference :: Float -> Float -> Float -> Float
wrappedDifference screenSize a b
    | difference > halfScreen = difference - screenSize
    | difference < -halfScreen = difference + screenSize
    | otherwise = difference
    where
        difference = a - b
        halfScreen = screenSize / 2









-- TODO:
-- Player:
--   Add screen wrapping. [x]
--   Add player lives and respawning. [o] (TODO respawnInvuln)
--   Add player destruction. [x]

-- Projectiles:
--   Implement bullet creation. [x]
--   Add projectile movement. [x]
--   Add projectile lifetime. [x]
--   Add bullet limit. [x] -> Cooldown is not actually implemented in the original seemingly so we wont either

-- Asteroids:
--   Implement asteroid data structures and movement. [x]
--   Add asteroid spawning. [x]
--   Add asteroid screen wrapping. [x] 
--   Implement asteroid destruction and splitting. [x]

-- Enemies:
--   Implement enemy saucers. [x]
--   Add saucer movement and behaviour. [x]
--   Add saucer spawning. [x]
--   Remove saucer crossing border [x]

-- Game mechanics:
--   Implement collision detection. [x] 
--   Implement collision responses. [x]
--   Add scoring.
--   Add difficulty progression.

-- Game flow:
--   Add game over state.
--   Add restart functionality.
