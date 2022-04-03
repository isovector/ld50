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

clampedArrows :: Controls -> V2 Int
clampedArrows = modifyX (clamp (0, 1)) . c_arrows


charpos :: Field -> SF FrameInfo (V2 Double)
charpos f = loopPre (V2 40 60) $ proc (FrameInfo controls dt, pos) -> do
  let dpos = fmap (* dt) . fmap (* 60) $ fmap fromIntegral $ clampedArrows controls
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
      composite (*>. drawText 4 blue "overlay" (V2 10 10)) $
        composite (drawText 12 black "underlay" (V2 10 80) *>.) $
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
asPerCamera cam@(V2 camx camy) pos =
  pos - cam + V2 (clamp (0, getX halfScreen + 50) camx)
                 (clamp (0, getY halfScreen + 4)  camy)

invertCamera :: V2 Double -> V2 Double -> V2 Double
invertCamera cam@(V2 camx camy) pos =
  pos + cam - V2 (clamp (0, getX halfScreen + 50) camx)
                 (clamp (0, getY halfScreen + 4)  camy)


field :: Resources -> SF FrameInfo Renderable
field rs = proc fi@(FrameInfo controls _) -> do
  now     <- time -< ()

  pos     <- charpos (r_fields rs TestField)     -< fi
  mc_run  <- playAnimation MainCharacter Run rs  -< now
  mc_idle <- playAnimation MainCharacter Idle rs -< now
  clap    <- playAnimation Claptrap NoAnim rs    -< now
  martha  <- playAnimation Martha Idle rs        -< now

  returnA -< \rs' -> do
    bgColor (V4 255 0 0 255) rs'
    let f = r_fields rs TestField
        cam = pos

    drawTiles f cam rs'

    let stretch = bool 0 (V2 2 0) $ getX (c_arrows controls) < 0
    drawSpriteStretched
      (bool mc_idle mc_run $ clampedArrows controls /= 0)
      (asPerCamera cam pos)
      0
      (pure False)
      stretch
      rs'

    drawSprite clap (asPerCamera cam $ V2 60 40) 0 (pure True) rs'

    let martha_pos = asPerCamera cam $ V2 80 80
    drawText 4 white "help!" (martha_pos - V2 0 30) rs
    drawSprite martha (martha_pos) 0 (V2 True False) rs'
    drawDarkness (round $ getX (asPerCamera cam pos)) rs'


drawTiles :: Field -> V2 Double -> Renderable
drawTiles f cam rs = do
  let tiles = f_data f
      topleft = worldToTile f $ invertCamera cam 0
      botright = worldToTile f $ invertCamera cam screen

  for_ [getY topleft .. getY botright] $ \y ->
    for_ [getX topleft .. getX botright] $ \x ->
      for_ (tiles x y) $ \wt ->
        drawSprite
          wt
          (asPerCamera cam $ (* f_tilesize f) $ fmap fromIntegral $ V2 x y)
          0
          (pure False)
          rs


drawDarkness :: Int -> Renderable
drawDarkness x rs = do
  let renderer = e_renderer $ r_engine rs
      darkness = r_textures rs Darkness
  copy
    renderer
    (getTexture darkness)
    Nothing
    (Just $ Rectangle (P $ V2 (fromIntegral x - 64) 0) $ fmap round $ screen * V2 0.5 1)
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer
    $ Just $ Rectangle (P $ V2 (fromIntegral x + 10) 0) (V2 1000 1000)


gameDfa :: ReaderT (Embedding FrameInfo Renderable FrameInfo Renderable) (Swont FrameInfo Renderable) ()
gameDfa = do
  over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
  stdWait $ \rs -> do
    bgColor (V4 50 0 50 255) rs
    drawText 8 white "-INTO DARKNESS-" (V2 20 20) rs
    drawText 8 white "by Sandy Maguire" (V2 15 120) rs

