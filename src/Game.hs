{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Data.Bool (bool)
import           Data.Foldable (for_)
import qualified Lens.Micro as L
import           Overlude hiding (now)
import           SDL hiding (delay, get, Event, time)
import Controls (waitControls)
import GHC.Generics (Generic)



charpos :: Field -> V2 Double -> SF FrameInfo (V2 Double)
charpos f p0 = loopPre p0 $ proc (FrameInfo controls dt, pos) -> do
  let dpos = fmap (* dt) . fmap (* 60) $ fmap fromIntegral $ setForce f $ c_arrows controls
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

data World = World
  { w_field :: FieldName
  , w_pos :: V2 Double
  }
  deriving Generic

game :: Resources -> SF FrameInfo Renderable
game rs =
  runCompositing (error "fin") $ runningGame rs


dialogMsg :: Compositing' FrameInfo Renderable ()
dialogMsg = liftCompositing $ do
  swont $ proc fi -> do
    msgs <- waitControls -< fi_controls fi
    returnA -<
      ( \rs -> do
          let renderer = e_renderer $ r_engine rs
          rendererDrawColor renderer $= pure 255
          fillRect renderer $ Just $ Rectangle (P $ pure 20) (pure 20)
      , bool noEvent (pure ()) $ any (== Ok) msgs
      )


runningGame :: Resources -> Compositing' FrameInfo Renderable World
runningGame rs = evolveC (World City $ V2 20 20) $ runningState rs

setForce :: Field -> V2 Int -> V2 Int
setForce f (V2 x y) =
  case f_force f of
    V2 fx fy -> V2 (force fx x) (force fy y)
  where
    force 0 b = b
    force a b
      | signum a /= signum b = 0
      | otherwise = b

facing :: (Num a, Ord a) => a -> a -> Maybe Bool
facing old new
  | old == new = Nothing
  | otherwise = Just $ new < old

direction :: SF (V2 Double) Bool
direction = (iPre 0 &&& arr id)
        >>> (arr $ \(old, new) -> maybeToEvent $ facing (getX old) (getX new))
        >>> dHold True

runningState
    :: Resources
    -> World
    -> Compositing' FrameInfo Renderable (World, Switch FrameInfo Renderable World)
runningState rs = \(World fname p0) ->
  let f = r_fields rs fname in
  withRoot $ dswont $ proc (root, L.over #fi_controls (bool (const defaultControls) id root) -> fi) -> do
    pos <- charpos f p0 -< fi
    anim    <- arr (bool Idle Run . (/= 0) . setForce f . c_arrows) -< fi_controls fi
    dir <- direction -< pos
    t <- mkAnim rs MainCharacter -< anim
    evs <- traverse zoneHandler $ f_zones f -< pos

    returnA -<
      ( const $ do
          drawTiles f pos rs
          drawSprite t (asPerCamera pos pos) 0 (V2 dir False) rs
      , mapFilterE (teleporter rs $ World fname pos) $ mergeEvents evs
      )

teleporter
    :: Resources
    -> World
    -> Message
    -> Maybe (World, Switch FrameInfo Renderable World)
teleporter rs w HitWall = Just (L.over #w_pos (+ V2 40 0) w, Bind $ runningState rs)
teleporter _ w Quit = Just (w, Push $ const $ dialogMsg >> pure (w, Done w) )
teleporter rs _ (Goto f v) = Just (World f v, Bind $ runningState rs)
teleporter _ _ _ = Nothing





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


zoneHandler :: Zone -> SF (V2 Double) (Event Message)
zoneHandler z@(Zone { z_type = SendMessage msg }) =
  proc pos -> do
    ev <- edgeTag msg -< rectContains (z_rect z) pos
    returnA -< ev


-- field :: Resources -> Swont (Bool, FrameInfo) Renderable [Message]
-- field rs = let f = r_fields rs Another in
--   swont $ proc (root, L.over #fi_controls (bool (const defaultControls) id root) -> fi@(FrameInfo controls _)) -> do
--   pos     <- charpos (r_fields rs Another) (V2 40 40) -< fi
--   anim    <- arr (bool Idle Run . (/= 0) . getX . clampedArrows) -< controls
--   mc      <- mkAnim rs MainCharacter -< anim

--   clap    <- mkAnim rs Claptrap      -< Idle
--   martha  <- mkAnim rs Martha        -< Idle

--   msgs <- traverse zoneHandler (f_zones f) -< pos

--   returnA -< (, sequenceA msgs) $ \rs' -> do
--     bgColor (V4 255 0 0 255) rs'
--     let cam = pos

--     drawTiles f cam rs'

--     let stretch = bool 0 (V2 2 0) $ getX (c_arrows controls) < 0
--     drawSpriteStretched
--       mc
--       (asPerCamera cam pos)
--       0
--       (pure False)
--       stretch
--       rs'

--     drawSprite clap (asPerCamera cam $ V2 60 40) 0 (pure True) rs'

--     let martha_pos = asPerCamera cam $ V2 80 80
--     drawText 4 white "help!" (martha_pos - V2 0 30) rs
--     drawSprite martha (martha_pos) 0 (V2 True False) rs'
--     drawDarkness (round $ getX (asPerCamera cam pos)) rs'


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


gameDfa :: Compositing' FrameInfo Renderable ()
gameDfa = do
  over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
  stdWait $ \rs -> do
    bgColor (V4 50 0 50 255) rs
    drawText 8 white "-INTO DARKNESS-" (V2 20 20) rs
    drawText 8 white "by Sandy Maguire" (V2 15 120) rs

