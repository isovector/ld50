{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall          #-}

module Main where

import Control.Monad
import Data.IORef
import Data.Time.Clock.System
import FRP.Yampa
import SDL hiding (copy, Stereo)
import System.Exit
import Foreign.C (CFloat)
import Types
import Game (game)

screenScale :: V2 CFloat
screenScale = V2 4 4

main :: IO ()
main = do
  initializeAll

  window <- createWindow "ld50" $ defaultWindow
    { windowInitialSize = fmap round $ (*) <$> screenScale <*> V2 160 144
    }
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedVSyncRenderer
    }
  rendererScale renderer $= screenScale
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  let engine = Engine
        { e_renderer = renderer
        }
      rs = Resources
        { r_engine = engine
        }

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure defaultControls)
    (input tRef)
    (output rs)
    game
  quit


input :: IORef Double -> Bool -> IO (DTime, Maybe Controls)
input tRef _ = do
  pumpEvents
  es <- pollEvents
  when (any (isQuit . eventPayload) es) exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  keys <- getKeyboardState

  let dt = min 0.016 (seconds' - seconds)
  pure (realToFrac dt, Just $ Controls $ keys ScancodeZ)


isQuit :: EventPayload -> Bool
isQuit QuitEvent             = True
isQuit (WindowClosedEvent _) = True
isQuit _                     = False


output :: Resources -> Bool -> Renderable -> IO Bool
output rs _ render = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render rs
  present renderer
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds tS = fromIntegral (systemSeconds tS) + fromIntegral (systemNanoseconds tS) / 1e9

