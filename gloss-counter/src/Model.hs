-- | This module contains the data types
--   which represent the state of the game
module Model where
import Data.Vector2

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
-- Temporary datastructure?
data Position = Position (Float, Float)



data ScreenState = Playing | Paused | Menu
data Projectile = Proj{
  enemy :: Bool
,  projPosition :: Position
,  projSpeed :: Vector2 Int
,  lifetime :: Int
}

data Enemy = Sauce Saucer | Aster Asteroid

data Saucer = Saucer {
  saucserPosition :: Position
, saucerSpeed :: Vector2 Int
, saucerSize :: Int
}

data Asteroid = Asteroid {
  asteroidPosition :: Position
, asteroidSpeed :: Vector2 Int
, asteroidSize :: Int
}

data Player = Player {
  playerPosition :: Position
, playerSpeed :: Vector2 Int
, bulletCount :: Int
, lives :: Int
}

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player :: Maybe Player
                 , enemies :: Maybe [Enemy]
                 , screenState :: Maybe ScreenState
                 , score :: Int
                 , projectiles :: Maybe [Projectile]
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 Nothing Nothing Nothing 0 Nothing