-- | This module contains the data types
--   which represent the state of the game
module Model where
import Data.Vector2
import Graphics.Gloss.Interface.IO.Game
import System.Random

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Position = Position (Float, Float) 
                  deriving Show

data ScreenState = Playing | Paused | Menu
                    deriving Eq
data Projectile = Proj{
  enemy :: Bool
, projPosition :: Position
, projSpeed :: Vector2 Float
, lifetime :: Float
}

data Enemy = Sauce Saucer | Aster Asteroid

data Saucer = Saucer {
  saucerPosition :: Position
, saucerSpeed :: Vector2 Float
, saucerSize :: Int
, saucerShootingCooldown :: Float
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


data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player :: Maybe Player
                 , enemies :: [Enemy]
                 , screenState :: ScreenState
                 , score :: Int
                 , projectiles :: [Projectile]
                 , heldKeys :: [Key]
                 , randomGenerator :: StdGen
                 }

initialState :: StdGen -> GameState
initialState gen = GameState ShowNothing 0 (Just initialPlayer) [] Playing 0 [] [] gen

initialPlayer :: Player
initialPlayer = Player
    { playerPosition = Position (0, 0)
    , playerSpeed = vector2 0 0
    , playerRotation = Rotation 0
    , bulletCount = 0
    , lives = 3
    }