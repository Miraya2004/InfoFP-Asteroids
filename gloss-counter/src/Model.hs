-- | This module contains the data types
--   which represent the state of the game
module Model where
import Data.Vector2

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Position = Position (Float, Float) 
                  deriving Show

data ScreenState = Playing | Paused | Menu
data Projectile = Proj{
  enemy :: Bool
, projPosition :: Position
, projSpeed :: Vector2 Float
, lifetime :: Int
}

data Enemy = Sauce Saucer | Aster Asteroid

data Saucer = Saucer {
  saucserPosition :: Position
, saucerSpeed :: Vector2 Float
, saucerSize :: Int
}

data Asteroid = Asteroid {
  asteroidPosition :: Position
, asteroidSpeed :: Vector2 Float
, asteroidSize :: Int
}

data Player = Player {
  playerPosition :: Position
, playerSpeed :: Vector2 Float
, playerRotation :: Rotation
, bulletCount :: Int
, lives :: Int
}
data Rotation = Rotation Float 
                  deriving Show

data Direction = DirLeft | DirRight

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player :: Maybe Player
                 , enemies :: [Enemy]
                 , screenState :: ScreenState
                 , score :: Int
                 , projectiles :: [Projectile]
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 (Just initialPlayer) [] Menu 0 []

initialPlayer :: Player
initialPlayer = Player
    { playerPosition = Position (0, 0)
    , playerSpeed = vector2 0 0
    , playerRotation = Rotation 0
    , bulletCount = 0
    , lives = 3
    }