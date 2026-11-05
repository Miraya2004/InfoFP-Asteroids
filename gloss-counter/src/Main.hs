module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do 
    gen <- newStdGen
    playIO (InWindow "Counter" (800, 600) (0, 0)) -- Or FullScreen
              black                 -- Background color
              10                    -- Frames per second
              (initialState gen)    -- Initial state
              view                  -- View function
              input                 -- Event function
              step                  -- Step function
