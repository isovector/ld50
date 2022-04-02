{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Overlude hiding (now)
import SDL hiding (time)
import Control.Monad.Reader


bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


charpos :: SF Controls (V2 Double)
charpos = loopPre (V2 0 0) $
  proc (controls, pos) -> do
    -- TODO(sandy): stupid and seems to be based on the frame rate
    let dpos = fmap (* 2) $ fmap fromIntegral $ c_arrows controls
    returnA -< dup $ pos + dpos


game :: Resources -> SF Controls Renderable
game rs = runSwont (runReaderT gameDfa (Embedding id)) $ const $
  proc controls -> do
    let bg = do
          case c_action controls of
            True -> bgColor $ V4 255 0 0 255
            False -> mempty
    now <- time -< ()

    pos <- charpos -< controls
    mc     <- playAnimation MainCharacter Run rs -< now
    clap   <- playAnimation Claptrap NoAnim rs   -< now
    martha <- playAnimation Martha Idle rs       -< now

    returnA -< \rs' -> do
      bg rs
      drawSprite mc pos 0 (pure False) rs'
      drawSprite clap (V2 @Float 60 40) 0 (pure True) rs'
      drawSprite martha (V2 @Float 80 80) 0 (V2 True False) rs'


gameDfa :: ReaderT (Embedding Controls Renderable Controls Renderable) (Swont Controls Renderable) ()
gameDfa = do
  over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
  stdWait $ \rs -> do
    bgColor (V4 50 0 50 255) rs
    drawText "-INTO DARKNESS-" (Point2 20 20) rs
    drawText "by Sandy Maguire" (Point2 15 120) rs

