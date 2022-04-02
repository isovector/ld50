{-# LANGUAGE Arrows #-}

module Game where

import Types
import SDL
import Control.Monad.Cont
import Control.Monad.Reader


bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


game :: SF Controls Renderable
game = runSwont (runReaderT gameDfa (Embedding id)) $ const $
  arr $ const $ bgColor $ V4 0 0 0 255


gameDfa :: ReaderT (Embedding a Renderable Controls Renderable) (Swont Controls Renderable) ()
gameDfa = do
  over 0.5 $ arr $ const $ bgColor $ V4 255 0 0 255
  over 0.5 $ arr $ const $ bgColor $ V4 0 255 0 255
  over 0.5 $ arr $ const $ bgColor $ V4 0 0 255 255



