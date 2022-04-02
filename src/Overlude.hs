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
import           Data.Point2
import qualified Data.Text as T
import           Foreign.C
import           SDL hiding (Event)
import           Types


compEmbed :: Embedding a' b' d e -> Embedding a b a' b' -> Embedding a b d e
compEmbed (Embedding g) (Embedding f) = Embedding (g . f)


embedArr :: (b -> e) -> Embedding d b d e
embedArr f = Embedding (>>> first (arr f))


embedInpArr :: (d -> a) -> Embedding a e d e
embedInpArr f = Embedding (arr f >>>)


runSwont :: Swont i o a -> (a -> SF i o) -> SF i o
runSwont x final = runCont (runSwont' x) final


over :: Time -> SF a b -> ReaderT (Embedding a b d e) (Swont d e) ()
over interval sf = do
  Embedding embed' <- ask
  lift . swont $ embed' $ sf &&& (after interval () >>> iPre NoEvent)

stdWaitFor :: (Message -> Bool) -> SF FrameInfo b -> ReaderT (Embedding FrameInfo b i o) (Swont i o) ()
stdWaitFor b sf = do
  Embedding embed' <- ask
  lift . swont $ embed' $ waitForMessage b sf


stdWait :: b -> ReaderT (Embedding FrameInfo b i o) (Swont i o) ()
stdWait sf = do
  Embedding embed' <- ask
  lift . swont $ embed' $ waitForOk $ constant sf

waitForMessage :: (Message -> Bool) -> SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForMessage b sf = sf &&& (arr fi_controls >>> waitControls >>> arr (any b) >>> edge)

waitForOk :: SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForOk = waitForMessage (== Ok)

waitForRestart :: SF FrameInfo o -> SF FrameInfo (o, Event ())
waitForRestart = waitForMessage (== Restart)


swont :: SF a (b, Event c) -> Swont a b c
swont = Swont . cont . switch


dswont :: SF a (b, Event c) -> Swont a b c
dswont = Swont . cont . dSwitch


always :: Arrow p => o -> p i o
always = arr . const


drawText :: String -> Point2 Double -> Resources -> IO ()
drawText text (Point2 (round -> x) (round -> y)) rs = do
  let renderer = e_renderer $ r_engine rs
  for_ (zip text [0..]) $ \(c, i) -> do
    let glyph = maybe (error $ "missing glyph " <> show c) id
              $ r_font rs c
    copy renderer glyph Nothing
      $ Just
      $ Rectangle (P $ V2 (x + i * 8) y)
      $ V2 8 8


timedSequence :: Double -> [SF i o] -> SF i o
timedSequence interval sfs =
  flip runSwont (error "timedSequence terminated") $
    traverse_ (dswont . (&&& after interval ())) sfs


playAnimation :: CharName -> Anim -> Resources -> SF Time WrappedTexture
playAnimation c a rs = timedSequence 0.1 $ cycle $ fmap always $ r_sprites rs c a

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


composite
    :: (d -> d)
    -> ReaderT (Embedding a b c d) (Swont i o) x
    -> ReaderT (Embedding a b c d) (Swont i o) x
composite f m = local (compEmbed $ embedArr f) m

(*>.) :: Applicative m => (a -> m b) -> (a -> m c) -> a -> m c
(*>.) = liftA2 (*>)

