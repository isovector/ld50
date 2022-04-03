{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}

module Overlude
  ( module Overlude
  , module Control.Monad
  , module Types
  , module Data.Point2
  , T.Text
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Controls
import           Data.Foldable (for_, traverse_)
import           Data.Functor ((<&>))
import           Data.Point2
import qualified Data.Text as T
import           Data.Void (Void, absurd)
import           Foreign.C
import           SDL hiding (time, Event)
import           Types


compEmbed :: Embedding a' b' d e -> Embedding a b a' b' -> Embedding a b d e
compEmbed (Embedding g) (Embedding f) = Embedding (g . f)


embedArr :: (b -> e) -> Embedding d b d e
embedArr f = Embedding (>>> first (arr f))


embedInpArr :: (d -> a) -> Embedding a e d e
embedInpArr f = Embedding (arr f >>>)


runSwont :: Swont i o a -> (a -> SF i o) -> SF i o
runSwont x final = runCont (runSwont' x) final

unCompositing :: FSM d e -> (a -> SF i o) -> Compositing d e i o a -> SF i o
unCompositing fsm g f
  = flip runSwont g
  $ flip runReaderT fsm $ getCompositing f

runCompositing :: (a -> SF i o) -> Compositing d e i o a -> SF i o
runCompositing = unCompositing (FSM (Embedding id) True)

liftCompositing :: Swont i o a -> Compositing d e i o a
liftCompositing = Compositing . ReaderT . const

liftSwont :: SF i o -> Swont i o b
liftSwont = swont . (&&& never)

withRoot :: Swont (Bool, i) o a -> Compositing d e i o a
withRoot m = Compositing $ ReaderT $ \fsm ->
  prependSwont (constant (fsm_root fsm) &&& arr id) (arr snd) m


over :: Time -> SF i o -> Compositing' i o ()
over interval sf = Compositing $ do
  Embedding embed' <- asks fsm_embedding
  lift . swont $ embed' $ sf &&& (after interval () >>> iPre NoEvent)

stdWaitFor :: (Message -> Bool) -> Compositing' FrameInfo o Void -> Compositing' FrameInfo o ()
stdWaitFor b sf = Compositing $ do
  fsm <- ask
  let Embedding embed' = fsm_embedding fsm
  lift . swont $ embed' $ waitForMessage b $ unCompositing fsm absurd sf


stdWait :: o -> Compositing' FrameInfo o ()
stdWait sf = Compositing $ do
  Embedding embed' <- asks fsm_embedding
  lift . swont $ embed' $ waitForOk $ constant sf

waitForMessage :: (Message -> Bool) -> SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForMessage b sf = sf &&& (arr fi_controls >>> waitControls >>> arr (any b) >>> edge)

waitForOk :: SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForOk = waitForMessage (== Ok)

waitForRestart :: SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForRestart = waitForMessage (== Restart)


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


timedSequence :: Double -> [SF i o] -> SF i o
timedSequence interval sfs =
  flip runSwont (error "timedSequence terminated") $
    traverse_ (dswont . (&&& after interval ())) sfs


frameSpeed :: CharName -> Anim -> Double
frameSpeed _ Idle = 0.5
frameSpeed _ _ = 0.1

drawSpriteStretched :: RealFloat a => WrappedTexture -> V2 a -> Double -> V2 Bool -> V2 Int -> Renderable
drawSpriteStretched wt pos theta flips stretch rs = do
  let renderer = e_renderer $ r_engine rs
      stretched = fmap fromIntegral stretch
  copyEx
    renderer
    (getTexture wt)
    (wt_sourceRect wt)
    (Just $ Rectangle (P $ fmap round pos - wt_origin wt - stretched) $ wt_size wt + stretched)
    (CDouble theta)
    (Just $ P $ wt_origin wt)
    flips

drawSprite :: RealFloat a => WrappedTexture -> V2 a -> Double -> V2 Bool -> Renderable
drawSprite wt pos theta flips = drawSpriteStretched wt pos theta flips 0

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


------------------------------------------------------------------------------
-- |  Lift a function over the FSM, but unset the root flag when doing so.
localFSM :: (Embedding' i o -> Embedding' i o) -> FSM i o -> FSM i o
localFSM f (FSM e _) = FSM (f e) False

composite
    :: (e -> e)
    -> Compositing d e i o a
    -> Compositing d e i o a
composite f (Compositing m) = Compositing $ local (localFSM $ compEmbed $ embedArr f) m

(*>.) :: Applicative m => (a -> m b) -> (a -> m c) -> a -> m c
(*>.) = liftA2 (*>)


sequenceFinite :: (Enum c, Bounded c, Applicative f) => (c -> f a) -> f (c -> a)
sequenceFinite f =
  let elems = [minBound .. maxBound]
      fas = fmap (zip elems) $sequenceA $ fmap f elems
   in fas <&> \cas c -> snd $ head $ drop (fromEnum c) cas

select :: (Enum c, Bounded c) => (c -> SF i o) -> SF (c, i) o
select f = proc (c, i) -> do
  rs <- sequenceFinite f -< i
  returnA -< rs c

mkAnim :: Resources -> CharName -> SF Anim WrappedTexture
mkAnim rs c = (arr id &&& time)
          >>> select (timedSequence 0.1 . cycle . fmap constant . r_sprites rs c)

