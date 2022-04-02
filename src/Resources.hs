module Resources where

import           Control.Monad
import           Data.Aeson.Tiled
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Traversable
import qualified Data.Vector as V
import           Overlude
import           SDL
import qualified SDL.Image as Image
import System.FilePath (dropFileName, takeDirectory)


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
    , wt_sourceRect = Nothing
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

fieldName :: FieldName -> String
fieldName TestField = "test"


framePath :: CharName -> Anim -> Int -> FilePath
framePath c a i =
  "./resources/sprites/" <> charName c <> "/" <> animName a <> "_" <> show i <> ".png"

fieldPath :: FieldName -> FilePath
fieldPath f = "./resources/maps/" <> fieldName f <> ".json"

globalToLocal :: Tileset -> GlobalId -> Maybe LocalId
globalToLocal ts gbl
  | gbl >= tilesetFirstgid ts
  = Just $ LocalId $ unGlobalId gbl - unGlobalId (tilesetFirstgid ts)
  | otherwise = Nothing


-- TODO(sandy): Super partial function. Sorry. But the tiled datastructure is
-- fucking insane.
parseTilemap :: Engine -> FieldName -> Tiledmap -> IO Field
parseTilemap e f ti = do
  let renderer = e_renderer e
  let layer = tiledmapLayers ti V.! 0
  tilesets <-
    for (tiledmapTilesets ti) $ \ts ->
      fmap (ts, ) . Image.loadTexture renderer
                  $ dropFileName (fieldPath f) <> tilesetImage ts
  let (ts, tx) = tilesets V.! 0

  pure $ Field $ \x y ->
    case ( within x 0 (tiledmapWidth ti) && within y 0 (tiledmapHeight ti)
         , layerData layer
         ) of
      (True, Just tiledata) -> do
        let idx = y * tiledmapWidth ti + x
            Just (LocalId tile) = globalToLocal ts $ tiledata V.! idx
            ix = tile `mod` tilesetColumns ts
            iy = tile `div` tilesetColumns ts
            -- tile = tilesetTiles ts M.! lcl
            size = fmap fromIntegral
                 $ V2 (tilesetTilewidth ts) (tilesetTileheight ts)
        Just $ WrappedTexture
          { getTexture = tx
          , wt_sourceRect = Just $ (Rectangle (P ((* size) $ fmap fromIntegral $ V2 ix iy)) size)
          , wt_size = size
          }
      _ -> Nothing

within :: Int -> Int -> Int -> Bool
within x lo hi = lo <= x && x < hi


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

  fields <- fmap M.fromList $
    for [minBound @FieldName .. maxBound] $ \fn -> do
      let fp = fieldPath fn
      loadTiledmap fp >>= \case
         Left s -> error s
         Right tm -> fmap (fn, ) $ parseTilemap e fn tm


  pure $ Resources
    { r_engine = e
    , r_font = flip M.lookup glyphs
    , r_sprites = curry $ fromMaybe [] . flip M.lookup chars
    , r_fields = fromMaybe (error "missing field data")
               . flip M.lookup fields
    }


