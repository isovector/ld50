{-# LANGUAGE Arrows #-}

module Game where

import Overlude
import SDL hiding (time)
import Control.Monad.Reader


bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


game :: SF Controls Renderable
game = runSwont (runReaderT gameDfa (Embedding id)) $ const $
  arr $ \c ->
    case c_action c of
      True -> bgColor $ V4 255 0 0 255
      False -> mempty


gameDfa :: ReaderT (Embedding Controls Renderable Controls Renderable) (Swont Controls Renderable) ()
gameDfa = do
  over 1 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * t) 0 0 255)
  stdWait $ bgColor $ V4 0 255 0 255
  over 0.5 $ always $ bgColor $ V4 0 0 255 255

