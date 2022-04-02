{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Overlude hiding (now)
import SDL hiding (time)
import Control.Monad.Reader
import Data.Foldable (for_)


bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


charpos :: SF FrameInfo (V2 Double)
charpos = loopPre (V2 0 0) $ proc (FrameInfo controls dt, pos) -> do
  let dpos = fmap (* dt) . fmap (* 60) $ fmap fromIntegral $ c_arrows controls
  returnA -< dup $ pos + dpos


game :: Resources -> SF FrameInfo Renderable
game rs
  = flip runSwont (const $ field rs)
  $ flip runReaderT (Embedding id) $ do
      composite (*>. drawText "overlay" (Point2 10 10)) $
        composite (drawText "underlay" (Point2 10 80) *>.) $
          stdWaitFor (== Restart) (field rs)
      gameDfa

field :: Resources -> SF FrameInfo Renderable
field rs = proc fi@(FrameInfo controls _) -> do
  let bg = do
        case c_action controls of
          True -> bgColor $ V4 255 0 0 255
          False -> mempty
  now <- time -< ()

  pos <- charpos -< fi
  mc     <- playAnimation MainCharacter Run rs -< now
  clap   <- playAnimation Claptrap NoAnim rs   -< now
  martha <- playAnimation Martha Idle rs       -< now

  returnA -< \rs' -> do
    bg rs
    let field = f_data $ r_fields rs TestField
    for_ [0 .. 16] $ \y ->
      for_ [0 .. 16] $ \x ->
        case field x y of
          Just wt ->
            drawSprite wt (fmap fromIntegral $ V2 x y * 16) 0 (pure False) rs'
          Nothing -> pure ()
    drawSprite mc pos 0 (pure False) rs'
    drawSprite clap (V2 @Float 60 40) 0 (pure True) rs'
    drawSprite martha (V2 @Float 80 80) 0 (V2 True False) rs'


gameDfa :: ReaderT (Embedding FrameInfo Renderable FrameInfo Renderable) (Swont FrameInfo Renderable) ()
gameDfa = do
  over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
  stdWait $ \rs -> do
    bgColor (V4 50 0 50 255) rs
    drawText "-INTO DARKNESS-" (Point2 20 20) rs
    drawText "by Sandy Maguire" (Point2 15 120) rs

