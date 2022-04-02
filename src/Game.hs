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


game :: Resources -> SF Controls Renderable
game rs = runSwont (runReaderT gameDfa (Embedding id)) $ const $
  proc controls -> do
    bg <- arr id -< arr $ do
      case c_action controls of
        True -> bgColor $ V4 255 0 0 255
        False -> mempty
    now <- time -< ()
    mc <- playAnimation MainCharacter Run rs -< now
    clap <- playAnimation Claptrap Idle rs -< now
    martha <- playAnimation Martha Idle rs -< now
    returnA -< \rs' -> do
      bg rs
      let renderer = e_renderer $ r_engine rs'
      copy renderer mc Nothing $ Just $ (Rectangle (P (V2 40 80)) (V2 16 24))
      copy renderer clap Nothing $ Just $ (Rectangle (P (V2 60 40)) (V2 16 24))
      copy renderer martha Nothing $ Just $ (Rectangle (P (V2 80 80)) (V2 16 24))
  -- ((arr $ \c -> do
  -- ) &&& _
  -- ) >>> _

gameDfa :: ReaderT (Embedding Controls Renderable Controls Renderable) (Swont Controls Renderable) ()
gameDfa = do
  pure ()
  -- over 1 $ time >>> arr (\t -> bgColor $ V4 (round $ 255 * t) 0 0 255)
  -- stdWait $ \rs -> do
  --   bgColor (V4 0 255 0 255) rs
  --   drawText "Sandy for president" (Point2 0 0) rs
  -- over 0.5 $ always $ bgColor $ V4 0 0 255 255

