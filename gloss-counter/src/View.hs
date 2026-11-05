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
shipStationary = Scale 3 3 $ Line [(0,5),(3,-5),(2,-4),(-2,-4),(-3,-5),(0,5)]
shipThruster :: Float -> Picture
shipThruster t = Scale 3 3 $ Line [(2,-4), (0,(-6 - flameSize)), (-2,-4)]
    where flameSize = cos $ fraction * 4 * pi
          fraction  = t - fromIntegral (floor t)

saucerSprite = Scale 2 2 $ Line [(-3,2),(3,2),(4,0),(-4,0),(-3,2),(-4,0),(-5,0),(-4,-2),(4,-2),(5,0),(4,0)]


asteroidSprites = Line [(-5,0), (0,5), (5, 0), (0, -5), (-5, 0)]




view ::GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Color white $ Pictures [renderShip gstate, renderEnemies gstate ]

renderEnemies :: GameState -> Picture
renderEnemies GameState{enemies = e} = Pictures $
      [Color white $ Text $ show $ length e] ++
      map renderEnemy e

renderEnemy :: Enemy -> Picture
renderEnemy (Aster a) = renderAsteroid a
renderEnemy (Sauce s) = renderSaucer s


renderSaucer :: Saucer -> Picture
renderSaucer Saucer {saucerPosition = Position (x,y), saucerSize = s}
         = Scale size size $ Translate y x $ Color white $ saucerSprite
            where size = fromIntegral $ s


renderShip :: GameState -> Picture
renderShip GameState {player = p, heldKeys = keys, elapsedTime = t} | isNothing p = Blank
                                                                    | isJust p    = Translate y x $ Rotate (rad2deg rot) $ shipSprite accel t
                                where Player {playerPosition = Position (x,y), playerRotation = Rotation rot} = fromMaybe initialPlayer p
                                      accel = elem (SpecialKey KeyUp) keys

shipSprite :: Bool -> Float -> Picture
shipSprite accel t | accel     = Pictures $ [shipThruster t, shipStationary]
                   | otherwise = shipStationary





renderAsteroids :: [Asteroid] -> [Picture]
renderAsteroids (a:as) = renderAsteroid a : renderAsteroids as
renderAsteroids [] = []

renderAsteroid Asteroid{asteroidPosition = Position (x,y), asteroidSize = sizeTier} = Scale size size $ Translate y x $ asteroidSprites
    where size = int2Float sizeTier



rad2deg :: Float -> Float
rad2deg r = 360 * r / (2 * pi) 