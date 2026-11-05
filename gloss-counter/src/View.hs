-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe
import GHC.Float
import Data.Fixed
import Graphics.Gloss.Interface.IO.Interact


shipStationary :: Picture
shipStationary = Rotate 90 $ Scale 3 3 $ Line [(0,5),(3,-5),(2,-4),(-2,-4),(-3,-5),(0,5)]
shipThruster :: Float -> Picture
shipThruster t = Rotate 90 $ Scale 3 3 $ Line [(2,-4), (0,(-6 - flameSize)), (-2,-4)]
    where flameSize = cos $ fraction * 4 * pi
          fraction  = t - fromIntegral (floor t)

saucerSprite = Scale 2 2 $ Line [(-3,2),(3,2),(4,0),(-4,0),(-3,2),(-4,0),(-5,0),(-4,-2),(4,-2),(5,0),(4,0)]


asteroidSprites = Line [(-5,0), (0,5), (5, 0), (0, -5), (-5, 0)]



renderScore :: Int -> Picture
renderScore score = Color white $ Scale 0.25 0.25 $ Text $ show score

renderLives :: Maybe Player -> Picture
renderLives (Just Player{lives = n}) = Color red $ live n

                                          where live 0 = Blank
                                                live n = Pictures [ThickCircle 5 5, Translate 15 0 $ live (n-1)]
                                                
renderLives Nothing = Blank

renderUI :: GameState -> Picture
renderUI GameState {score = score, player = player, highestScore = highestScore, screenState = screenState} = Translate (negate 370) (250) $ Pictures [
                                                            renderScore score,
                                                            Translate 0 (negate 10) $ renderLives player,
                                                            renderEndScreen player score (fromMaybe 0 highestScore),
                                                            renderResume screenState
                                                      ]

renderResume :: ScreenState -> Picture
renderResume Paused = Color white $ Translate (negate 0) (negate 400)     $ Scale 0.5 0.5 $ Text "Press P to resume"
renderResume _      = Blank

renderEndScreen :: Maybe Player -> Int -> Int -> Picture
renderEndScreen (Just _) _ _ = blank
renderEndScreen Nothing score highest = Pictures [
                                Color white $ Translate (negate 25) (negate 100) $ Scale 1 1       $ Text "GAME OVER",
                                if highest <= score then Color white $ Translate (negate 0) (negate 300)     $ Scale 0.5 0.5 $ Text "NEW HIGH SCORE"
                                else Color white $ Translate (negate 0) (negate 300)     $ Scale 0.5 0.5 $ Text ("HIGH SCORE:" ++ show highest),
                                Color orange $ Translate (negate 0) (negate 400)     $ Scale 0.5 0.5 $ Text "Press C to continue",
                                Color green $ Translate (negate 0) (negate 500)     $ Scale 0.5 0.5 $ Text "or R to restart"
                              ]

renderProjectiles :: GameState -> Picture
renderProjectiles GameState {projectiles = p} = Pictures $ map renderProjectile p

renderProjectile :: Projectile -> Picture
renderProjectile Proj{projPosition = Position(x,y), enemy = evil} | evil      = Translate x (negate y) $ Color red $ Circle 3
                                                                  | otherwise = Translate x (negate y) $ Color white $ Circle 3


view ::GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Color white $ Pictures [renderShip gstate, renderEnemies gstate, renderProjectiles gstate, renderUI gstate]

renderEnemies :: GameState -> Picture
renderEnemies GameState{enemies = e} = Pictures $ map renderEnemy e

renderEnemy :: Enemy -> Picture
renderEnemy (Aster a) = renderAsteroid a
renderEnemy (Sauce s) = renderSaucer s


renderSaucer :: Saucer -> Picture
renderSaucer Saucer {saucerPosition = Position (x,y), saucerSize = s}
         = Translate x (negate y) $ Scale size size $ Color white $ saucerSprite
            where size = fromIntegral s


renderShip :: GameState -> Picture
renderShip GameState {player = p, heldKeys = keys, elapsedTime = t} | isNothing p = Blank
                                                                    | isJust p    = Translate x (negate y) $ Rotate (rad2deg rot) $ shipSprite accel t
                                where Player {playerPosition = Position (x,y), playerRotation = Rotation rot} = fromMaybe initialPlayer p
                                      accel = elem (SpecialKey KeyUp) keys

shipSprite :: Bool -> Float -> Picture
shipSprite accel t | accel     = Pictures $ [shipThruster t, shipStationary]
                   | otherwise = shipStationary





renderAsteroids :: [Asteroid] -> [Picture]
renderAsteroids (a:as) = renderAsteroid a : renderAsteroids as
renderAsteroids [] = []

renderAsteroid Asteroid{asteroidPosition = Position (x,y), asteroidSize = sizeTier, asteroidLifetime = t} | sizeTier > 0  =  Translate x (negate y) $ Scale size size $ asteroidSprites
                                                                                                          | sizeTier == 0 = Translate x (negate y) $ ThickCircle (3 - t / 50) (3 - t/100)
    where size = fromIntegral sizeTier



rad2deg :: Float -> Float
rad2deg r = 360 * r / (2 * pi) 