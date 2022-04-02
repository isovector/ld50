{-# LANGUAGE ViewPatterns #-}
module Overlude
  ( module Overlude
  , module Types
  , module Data.Point2
  , T.Text
  ) where

import           Control.Monad.Cont
import           Control.Monad.Reader
import           Controls
import           Data.Foldable (for_)
import           Data.Point2
import qualified Data.Text as T
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

stdWait :: b -> ReaderT (Embedding Controls b i o) (Swont i o) ()
stdWait sf = do
  Embedding embed' <- ask
  lift . swont $ embed' $ wait sf

wait :: c -> SF Controls (c, Event ())
wait sf = constant sf &&& (waitControls >>> arr (not . null) >>> edge)


swont :: SF a (b, Event c) -> Swont a b c
swont = Swont . cont . switch

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

