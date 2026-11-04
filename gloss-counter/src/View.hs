-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe

shipStationary = Line [(0,5),(3,-5),(2,-4),(-2,-4),(-3,-5),(0,5)]
shipThruster = Line [(2,-4), (0,-6), (-2,-4)]


asteroidSprites = Line [(-2,0), (0,2), (2, 0), (0, -2), (-2, 0)]




view ::GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Color white $ Scale 10 10 $ Pictures [renderShip gstate   ]




renderShip :: GameState -> Picture
renderShip GameState {player = p} | isNothing p = Blank
                                  | isJust p    = Translate x y $ Rotate (rot) $ shipSprite (accel)
                                where Player {accelerating = accel, playerPosition = Position (x,y), playerRotation = Rotation rot} = fromMaybe initialPlayer p

shipSprite :: Bool -> Picture
shipSprite accel | accel     = Pictures $ [shipThruster, shipStationary]
                 | otherwise = shipStationary