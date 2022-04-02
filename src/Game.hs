{-# LANGUAGE Arrows #-}

module Game where

import Types
import SDL

game :: SF Controls Renderable
game = arr $ \c rs ->
  case c_action c of
    False -> pure ()
    True -> do
      let renderer = e_renderer $ r_engine rs
      rendererDrawColor renderer $= V4 255 0 0 255
      clear renderer

