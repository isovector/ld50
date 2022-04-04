{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Data.Bool (bool)
import           Data.Foldable (for_, asum)
import           GHC.Generics (Generic)
import qualified Lens.Micro as L
import           Overlude
import           SDL hiding (delay, get, Event, time)
import Prelude hiding (interact)


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
  runCompositing (error "fin") $ do
    -- dialogMsg Martha "Sandy is awesome"
    runningGame rs


dialogMsg :: CharName -> String -> Compositing' FrameInfo Renderable ()
dialogMsg who say = liftCompositing $ do
  momentary $ maybe (const $ pure ()) playSound $ charVoice who
  swont $ proc fi -> do
    ok <- interactionEvent -< fi_controls fi
    returnA -<
      ( \rs -> do
          let renderer = e_renderer $ r_engine rs
          rendererDrawColor renderer $= V4 0 0 0 192
          fillRect renderer $ Just $ Rectangle (P $ V2 0 95) $ fmap round screen
          drawPortait who (V2 2 100) rs
          drawText 6 white say (V2 42 110) rs
      , ok
      )

momentary :: (Resources -> IO ()) -> Swont i (Resources -> IO ()) ()
momentary what = do
  -- TODO(sandy): not sure why this is necessary, but IO actions dont seem to
  -- happen otherwise
  swont $ after 0.001 () >>> (constant (const $ pure ()) &&& arr id)
  dswont $ constant what &&& now ()


runningGame :: Resources -> Compositing' FrameInfo Renderable World
runningGame rs = evolveC (World City $ V2 80 140) $ runningState rs

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

facingToFacing :: Facing -> V2 Bool
facingToFacing FacingLeft = V2 True False
facingToFacing FacingRight = pure False

interactionEvent :: SF Controls (Event ())
interactionEvent = arr c_action >>> edge


runningState
    :: Resources
    -> World
    -> Compositing' FrameInfo Renderable (World, Switch FrameInfo Renderable World)
runningState rs = \(World fname p0) ->
  let f = r_fields rs fname in
  withRoot $ dswont $ proc (root, L.over #fi_controls (bool (const defaultControls) id root) -> fi) -> do
    pos  <- charpos f p0 -< fi
    anim <- arr (bool Idle Run . (/= 0) . setForce f . c_arrows) -< fi_controls fi
    dir  <- direction -< pos
    t    <- mkAnim rs MainCharacter -< anim

    peeps <- traverse (mkAnim rs . a_name) (f_actors f) -< Idle
    evs   <- traverse zoneHandler $ f_zones f -< pos
    interact <- interactionEvent -< fi_controls fi
    to_play <- soundTrigger Hit -< interact

    returnA -<
      ( const $ do
          let cam = pos
          -- to_play rs

          drawTiles f pos rs

          for_ (zip peeps $ f_actors f) $ \(wt, actor) -> do
            drawSprite wt
              (asPerCamera f cam $ a_pos actor)
              0
              (facingToFacing $ a_facing actor)
              rs

          drawSprite t (asPerCamera f cam pos) 0 (V2 dir False) rs
          when (f_force f /= 0) $
            drawDarkness (round $ getX (asPerCamera f cam pos)) rs

      , let w' = World fname pos in
        mergeEvents
          [ mapFilterE (teleporter rs w') $ mergeEvents evs
          , mapFilterE (runInteraction w') $ do
              () <- interact
              asum $ do
                actor <- f_actors f
                case a_interaction actor of
                  Just i ->
                    if norm (a_pos actor - pos) <= 20
                      then pure $ Event i
                      else empty
                  Nothing -> empty
          ]
      )

teleporter
    :: Resources
    -> World
    -> WorldInteraction
    -> Maybe (World, Switch FrameInfo Renderable World)
teleporter rs _ (Goto f v) =
  -- This is a bad fade, but whatever.
  Just $ (World f v,) $ Bind $ \w ->
    pushDialog (runningState rs w) $ do
      over 0.5 $ fadeTo (V4 0 0 0 255) 0.5
      over 0.5 $ fadeFrom (V4 0 0 0 255) 0.5

runInteraction
    :: World
    -> ActorInteraction
    -> Maybe (World, Switch FrameInfo Renderable World)
runInteraction w PortalWarning =
  Just $ (w,) $ Push $ const $ do
    dialogMsg Martha "Beware the portal!"
    dialogMsg MainCharacter "Odd..."
    pure (w, Done w)




worldToTile :: Field -> V2 Double -> V2 Int
worldToTile f pos =
  fmap floor $ pos * fmap (1 /) (f_tilesize f)


screen :: V2 Double
screen = V2 160 144

halfScreen :: V2 Double
halfScreen = screen * 0.5

asPerCamera :: Field -> V2 Double -> V2 Double -> V2 Double
asPerCamera f cam@(V2 camx camy) pos =
  pos - cam + V2 (clamp (0, getX offset) camx)
                 (clamp (0, getY offset) camy)
  where
    offset = halfScreen + V2 0 4 + fmap (fromIntegral) (f_force f) * 50


invertCamera :: Field -> V2 Double -> V2 Double -> V2 Double
invertCamera f cam@(V2 camx camy) pos =
  pos + cam - V2 (clamp (0, getX offset) camx)
                 (clamp (0, getY offset) camy)
  where
    offset = halfScreen + V2 0 4 + fmap (fromIntegral) (f_force f) * 50


zoneHandler :: Zone -> SF (V2 Double) (Event WorldInteraction)
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


drawTiles :: Field -> V2 Double -> Renderable
drawTiles f cam rs = do
  let tiles = f_data f
      topleft = worldToTile f $ invertCamera f cam 0
      botright = worldToTile f $ invertCamera f cam screen

  for_ [getY topleft .. getY botright] $ \y ->
    for_ [getX topleft .. getX botright] $ \x ->
      for_ (tiles x y) $ \wt ->
        drawSprite
          wt
          (asPerCamera f cam $ (* f_tilesize f) $ fmap fromIntegral $ V2 x y)
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


-- gameDfa :: Compositing' FrameInfo Renderable ()
-- gameDfa = do
--   over 1.9 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * (1 - t / 2)) 0 0 255)
--   stdWait $ \rs -> do
--     bgColor (V4 50 0 50 255) rs
--     drawText 8 white "-INTO DARKNESS-" (V2 20 20) rs
--     drawText 8 white "by Sandy Maguire" (V2 15 120) rs


portrait :: Resources -> CharName -> WrappedTexture
portrait rs c =
  (head $ r_sprites rs c NoAnim)
    { wt_sourceRect = Just (Rectangle (P $ V2 2 6) 14)
    , wt_size = 14
    , wt_origin = 0
    }

drawPortait :: CharName -> V2 Double -> Renderable
drawPortait c pos rs =
  drawSpriteStretched (portrait rs c) pos 0 (pure False) 3 rs

charVoice :: CharName -> Maybe Sound
charVoice MainCharacter = Just MCSay
charVoice Martha = Just MarthaSay
charVoice Claptrap = Nothing

