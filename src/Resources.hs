module Resources where

import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Traversable
import           Overlude
import           SDL
import qualified SDL.Image as Image


pad :: Int -> Char -> String -> String
pad n c s =
  let len = length s
   in case len >= n of
        True -> s
        False -> replicate (n - len) c <> s


wrapTexture :: Texture -> IO WrappedTexture
wrapTexture t = do
  q <- queryTexture t
  pure $ WrappedTexture
    { getTexture = t
    , wt_size = V2 (textureWidth q) $ textureHeight q
    }


frameCounts :: CharName -> Anim -> Int
frameCounts _ Idle   = 4
frameCounts _ NoAnim = 1
frameCounts _ Run    = 4


charName :: CharName -> String
charName MainCharacter = "mc"
charName Martha        = "martha"
charName Claptrap      = "claptrap"


animName :: Anim -> String
animName Idle   = "idle"
animName NoAnim = "no_anim"
animName Run    = "run"


framePath :: CharName -> Anim -> Int -> FilePath
framePath c a i =
  "resources/sprites/" <> charName c <> "/" <> animName a <> "_" <> show i <> ".png"


loadResources :: Engine -> IO Resources
loadResources e = do
  let renderer = e_renderer e
  glyphs <-
    fmap M.fromList $ for [32 .. 122] $ \code -> do
      let fp = "resources/font/font-" <> pad 3 '0' (show code) <> ".png"
      texture <- Image.loadTexture renderer fp
      pure (toEnum @Char code, texture)

  chars <- fmap (M.fromList . join) $
    for [minBound @CharName .. maxBound] $ \char ->
      for [minBound @Anim .. maxBound] $ \anim ->  do
        frames <- for [0 .. frameCounts char anim - 1] $ \i -> do
          let fp = framePath char anim i
          wrapTexture =<< Image.loadTexture renderer fp
        pure ((char, anim), frames)

  pure $ Resources
    { r_engine = e
    , r_font = flip M.lookup glyphs
    , r_sprites = curry $ fromMaybe [] . flip M.lookup chars
    }


