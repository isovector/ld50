module Overlude
  ( module Overlude
  , module Control.Monad
  , module Types
  , module Data.Point2
  , T.Text
  , ask
  , asks
  ) where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Data.Foldable (for_, traverse_)
import           Data.Point2
import qualified Data.Text as T
import           Foreign.C
import           SDL hiding (time, Event)
import           Types hiding (next)
import Data.List (genericLength)

bgColor :: V4 Word8 -> Renderable
bgColor col rs = do
  let renderer = e_renderer $ r_engine rs
  rendererDrawColor renderer $= col
  clear renderer


compEmbed :: Embedding a' b' d e -> Embedding a b a' b' -> Embedding a b d e
compEmbed (Embedding g) (Embedding f) = Embedding (g . f)

liftEmbed :: SF i o -> Embedding a i a o
liftEmbed io = Embedding $ \ai -> ai >>> io

liftInpEmbed :: SF i o -> Embedding o b i b
liftInpEmbed io = Embedding $ \b -> io >>> b


embedArr :: (b -> e) -> Embedding d b d e
embedArr = liftEmbed . arr


embedInpArr :: (d -> a) -> Embedding a e d e
embedInpArr = liftInpEmbed . arr


runSwont :: Swont i o a -> (a -> SF i o) -> SF i o
runSwont x final = runCont (runSwont' x) final

unCompositing :: FSM d e -> (a -> SF i o) -> Compositing d e i o a -> SF i o
unCompositing fsm g
  = flip runSwont g
  . flip runReaderT fsm
  . getCompositing

invMapCont :: (r -> r') -> (r' -> r) -> Cont r a -> Cont r' a
invMapCont to from ct = cont $ \k -> to $ runCont ct $ from . k


runCompositing :: (a -> SF i Renderable) -> Compositing' i Renderable a -> SF i Renderable
runCompositing = unCompositing (FSM (Embedding id) True)

liftCompositing :: Swont i o a -> Compositing' i o a
liftCompositing s = Compositing $ do
  Embedding embed' <- asks fsm_embedding
  lift $ Swont $ cont $ \k -> embed' $ runSwont s k

liftSwont :: SF i o -> Swont i o b
liftSwont = swont . (&&& never)

withRoot :: Swont (Bool, i) o a -> Compositing d e i o a
withRoot m = Compositing $ ReaderT $ \fsm ->
  prependSwont (constant (fsm_root fsm) &&& arr id) (arr snd) m



over :: Time -> SF i o -> Compositing' i o ()
over interval sf = Compositing $ do
  Embedding embed' <- asks fsm_embedding
  lift $ swont $ embed' sf &&& (after interval () >>> iPre NoEvent)

-- stdWaitFor :: (Message -> Bool) -> Compositing' FrameInfo o Void -> Compositing' FrameInfo o ()
-- stdWaitFor b sf = Compositing $ do
--   fsm <- ask
--   let Embedding embed' = fsm_embedding fsm
--   lift . swont $ waitForMessage b $ embed' $ unCompositing fsm absurd sf


-- stdWait :: o -> Compositing' FrameInfo o ()
-- stdWait sf = Compositing $ do
--   Embedding embed' <- asks fsm_embedding
--   lift . swont $ waitForOk $ embed' $ constant sf

-- waitForMessage :: (Message -> Bool) -> SF FrameInfo o -> SF FrameInfo (o, Event ())
-- waitForMessage b sf = sf &&& (arr fi_controls >>> waitControls >>> arr (any b) >>> edge)

-- waitForOk :: SF FrameInfo o -> SF FrameInfo (o, Event ())
-- waitForOk = waitForMessage (== Ok)

-- waitForRestart :: SF FrameInfo o -> SF FrameInfo (o, Event ())
-- waitForRestart = waitForMessage (== Restart)


------------------------------------------------------------------------------
-- | 'swont' embeds an SF that returns an event into a 'Swont'. The
-- continuation is called when the event fires. This is the primary means of
-- sequencing things in the 'Swont' monad.
swont :: SF a (b, Event c) -> Swont a b c
swont = Swont . cont . switch


dswont :: SF a (b, Event c) -> Swont a b c
dswont = Swont . cont . dSwitch

prependSwont :: SF i i' -> SF i' i -> Swont i' o a -> Swont i o a
prependSwont ii' i'i (Swont (runCont -> io)) = Swont $ cont $ \k -> ii' >>> io (\a -> i'i >>> k a)

-- extendSwont :: Swont i o a -> SF o o' -> Swont i o' a
-- extendSwont (Swont (runCont -> io)) oo' = Swont $ cont $ \k -> io (\a -> _) >>> oo'



drawText :: CInt -> V3 Word8 -> String -> V2 Double -> Resources -> IO ()
drawText sz color text (V2 x y) rs = do
  let renderer = e_renderer $ r_engine rs
  for_ (zip text [0..]) $ \(c, i) -> do
    let glyph = maybe (error $ "missing glyph " <> show c) id
              $ r_font rs c
    textureColorMod glyph $= color
    copy renderer glyph Nothing
      $ Just
      $ Rectangle (P $ fmap round $ V2 (x + fromIntegral (i * sz)) y)
      $ V2 sz sz
  rendererDrawBlendMode renderer $= BlendAlphaBlend


timedSequence :: SF i o -> Double -> [SF i o] -> SF i o
timedSequence d interval sfs =
  flip runSwont (const d) $
    traverse_ (dswont . (&&& after interval ())) sfs


frameSpeed :: CharName -> Anim -> Double
frameSpeed _ Idle = 0.5
frameSpeed _ _ = 0.1

drawSpriteStretched :: WrappedTexture -> V2 Double -> Double -> V2 Bool -> V2 Double -> Renderable
drawSpriteStretched wt pos theta flips stretched rs = do
  let renderer = e_renderer $ r_engine rs
  copyEx
    renderer
    (getTexture wt)
    (wt_sourceRect wt)
    (Just $ fmap round
          $ Rectangle (P $ pos - (fmap fromIntegral (wt_origin wt) * stretched))
          $ fmap fromIntegral (wt_size wt) * stretched)
    (CDouble theta)
    (Just $ fmap round
          $ P
          $ fmap fromIntegral (wt_origin wt) * stretched)
    flips

drawSprite :: WrappedTexture -> V2 Double -> Double -> V2 Bool -> Renderable
drawSprite wt pos theta flips = drawSpriteStretched wt pos theta flips 1

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) a = min hi $ max lo a

getX :: V2 a -> a
getX (V2 x _) = x

modifyX :: (a -> a) -> V2 a -> V2 a
modifyX f (V2 x y) = V2 (f x) y

getY :: V2 a -> a
getY (V2 _ y) = y

modifyY :: (a -> a) -> V2 a -> V2 a
modifyY f (V2 x y) = V2 x (f y)

rectContains :: (Ord a, Num a) => Rectangle a -> V2 a -> Bool
rectContains (Rectangle (P (V2 x y)) (V2 w h)) (V2 px py) = and
  [ x <= px
  , px < x + w
  , y <= py
  , py < y + h
  ]


evolve :: s -> (s -> Swont i o (s, Maybe e)) -> Swont i o (s, e)
evolve s0 f =
  f s0 >>= \case
    (s, Just e) -> pure (s, e)
    (s, Nothing) -> evolve s f

evolveC :: s -> (s -> Compositing' i Renderable (s, Switch i Renderable s)) -> Compositing' i Renderable s
evolveC s0 f = do
  (s, next) <- f s0
  case next of
    Push g -> pushDialog (evolveC s f) $ evolveC s g
    Bind g -> evolveC s g
    Done r -> pure r

pushDialog :: Compositing' i Renderable b -> Compositing' i Renderable a -> Compositing' i Renderable b
pushDialog under above = do
  fsm <- ask
  _ <- push (*>.) (unCompositing (localFSM id fsm) (error "sf ended during dialog") under) above
  under


------------------------------------------------------------------------------
-- |  Lift a function over the FSM, but unset the root flag when doing so.
localFSM :: (Embedding' i o -> Embedding' i o) -> FSM i o -> FSM i o
localFSM f (FSM e _) = FSM (f e) False

composite
    :: (e -> e)
    -> Compositing d e i o a
    -> Compositing d e i o a
composite f (Compositing m) = Compositing $ local (localFSM $ compEmbed $ embedArr f) m


push
    :: (e -> e -> e)
    -> SF d e
    -> Compositing d e i o a
    -> Compositing d e i o a
push combine g above =
  Compositing $ flip local (getCompositing above) $ localFSM $ compEmbed $
    Embedding $ \f ->
      proc a -> do
        e1 <- f -< a
        e2 <- g -< a
        returnA -< combine e2 e1

(*>.) :: Applicative m => (a -> m b) -> (a -> m c) -> a -> m c
(*>.) = liftA2 (*>)


select :: (Enum c, Bounded c) => (c -> SF i o) -> SF (c, i) o
select f = proc (c, i) -> do
  rs <- parB f -< i
  returnA -< rs c

mkAnim :: Resources -> CharName -> SF Anim WrappedTexture
mkAnim rs c = (arr id &&& time)
          >>> select (timedSequence (error "impossible") 0.1 . cycle . fmap constant . r_sprites rs c)


fade :: [V4 Word8] -> Time -> SF a Renderable
fade colors duration =
  timedSequence
      (constant $ const $ pure ())
      (duration / genericLength colors) $ colors <&> \c -> constant $ \rs -> do
    let renderer = e_renderer $ r_engine rs
    rendererDrawColor renderer $= c
    fillRect renderer Nothing

fadeColors :: V4 Word8 -> [V4 Word8]
fadeColors (V4 r g b a) =
  [ V4 r g b (a `div` 4)
  , V4 r g b (a `div` 2)
  , V4 r g b a
  , V4 r g b a
  ]

fadeTo :: V4 Word8 -> Double -> SF a Renderable
fadeTo color = fade (fadeColors color)

fadeFrom :: V4 Word8 -> Double -> SF a Renderable
fadeFrom color = fade (reverse $ fadeColors color)

