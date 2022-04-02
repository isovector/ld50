{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Overlude hiding (now)
import SDL hiding (time)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Bool (bool)


bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


charpos :: Field -> SF FrameInfo (V2 Double)
charpos f = loopPre (V2 0 40) $ proc (FrameInfo controls dt, pos) -> do
  let dpos = fmap (* dt) . fmap (* 60) $ fmap fromIntegral $ modifyX (clamp (0, 1)) $ c_arrows controls
  returnA -< dup $
    let pos' = pos + dpos
     in case f_walkable f (worldToTile f pos') of
          True -> pos'
          False -> pos

blue :: V3 Word8
blue = V3 0 0 255

white :: V3 Word8
white = V3 255 255 255

black :: V3 Word8
black = V3 0 0 0

game :: Resources -> SF FrameInfo Renderable
game rs
  = flip runSwont (const $ field rs)
  $ flip runReaderT (Embedding id) $ do
      composite (*>. drawText 4 blue "overlay" (Point2 10 10)) $
        composite (drawText 12 black "underlay" (Point2 10 80) *>.) $
          stdWaitFor (== Restart) (field rs)
      gameDfa

worldToTile :: Field -> V2 Double -> V2 Int
worldToTile f pos =
  fmap floor $ pos * fmap (1 /) (f_tilesize f)


screen :: V2 Double
screen = V2 160 144

halfScreen :: V2 Double
halfScreen = screen * 0.5

asPerCamera :: V2 Double -> V2 Double -> V2 Double
asPerCamera cam pos = pos - cam + halfScreen

field :: Resources -> SF FrameInfo Renderable
field rs = proc fi@(FrameInfo controls _) -> do
  let bg = do
        case c_action controls of
          True -> bgColor $ V4 255 0 0 255
          False -> mempty
  now <- time -< ()

  pos <- charpos (r_fields rs TestField) -< fi
  mc     <- playAnimation MainCharacter Run rs -< now
  clap   <- playAnimation Claptrap NoAnim rs   -< now
  martha <- playAnimation Martha Idle rs       -< now

  returnA -< \rs' -> do
    bg rs
    let f = r_fields rs TestField
        tiles = f_data f

    let cam = pos

    for_ [0 .. 16] $ \y ->
      for_ [0 .. 16] $ \x ->
        case tiles x y of
          Just wt ->
            drawSprite wt (asPerCamera cam $ (* f_tilesize f) $ fmap fromIntegral $ V2 x y) 0 (pure False) rs'
          Nothing -> pure ()
    let stretch = bool 0 (V2 5 0) $ getX (c_arrows controls) < 0
    drawSpriteStretched mc (asPerCamera cam pos) 0 (pure False) stretch rs'
    drawSprite clap (asPerCamera cam $ V2 60 40) 0 (pure True) rs'
    drawSprite martha (asPerCamera cam $ V2 80 80) 0 (V2 True False) rs'
    drawDarkness (round $ getX (asPerCamera cam pos) + 4) rs'


drawDarkness :: Int -> Renderable
drawDarkness x rs = do
  let renderer = e_renderer $ r_engine rs
  copy
    renderer
    (getTexture $ r_textures rs Darkness)
    Nothing
    (Just $ Rectangle (P $ V2 (fromIntegral x) 0) (V2 10 200))
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer
    $ Just $ Rectangle (P $ V2 (fromIntegral x + 10) 0) (V2 1000 1000)


gameDfa :: ReaderT (Embedding FrameInfo Renderable FrameInfo Renderable) (Swont FrameInfo Renderable) ()
gameDfa = do
  over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
  stdWait $ \rs -> do
    bgColor (V4 50 0 50 255) rs
    drawText 8 white "-INTO DARKNESS-" (Point2 20 20) rs
    drawText 8 white "by Sandy Maguire" (Point2 15 120) rs

