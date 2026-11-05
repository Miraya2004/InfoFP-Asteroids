-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe
import GHC.Float


shipStationary = Line [(0,5),(3,-5),(2,-4),(-2,-4),(-3,-5),(0,5)]
shipThruster = Line [(2,-4), (0,-6), (-2,-4)]


asteroidSprites = Line [(-2,0), (0,2), (2, 0), (0, -2), (-2, 0)]




view ::GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Color white $ Pictures [renderShip gstate   ]

renderEnemies :: GameState -> Picture
renderEnemies GameState{enemies = e} = Pictures $ 
      renderAsteroids . map (\(Aster a) -> a) $ (filter isAsteroid) (e)
      --renderSaucers . map (\(Sauce s) -> s) . (filter isSaucer) (e)


isAsteroid :: Enemy -> Bool
isAsteroid (Aster _) = True
isAsteroid _         = False

isSaucer :: Enemy -> Bool
isSaucer (Sauce _) = True
isSaucer _         = False


renderShip :: GameState -> Picture
renderShip GameState {player = p} | isNothing p = Blank
                                  | isJust p    = Translate y x $ scale 3 3 $ Rotate (rad2deg rot) $ shipSprite (accel)
                                where Player {accelerating = accel, playerPosition = Position (x,y), playerRotation = Rotation rot} = fromMaybe initialPlayer p

shipSprite :: Bool -> Picture
shipSprite accel | accel     = Pictures $ [shipThruster, shipStationary]
                 | otherwise = shipStationary



renderAsteroids :: [Asteroid] -> [Picture]
renderAsteroids (a:as) = renderAsteroid a : renderAsteroids as
renderAsteroids [] = []

renderAsteroid Asteroid{asteroidPosition = Position (x,y), asteroidSize = sizeTier} = Scale size size $ Translate x y $ asteroidSprites
    where size = 10 * int2Float sizeTier



rad2deg :: Float -> Float
rad2deg r = 360 * r / (2 * pi) 