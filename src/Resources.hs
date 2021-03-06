{-# LANGUAGE OverloadedStrings #-}

module Resources where

import           Control.Monad
import           Data.Aeson hiding (Object)
import           Data.Aeson.Tiled
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList, mapMaybe)
import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           GHC.Stack (HasCallStack)
import           Overlude
import           SDL
import qualified SDL.Image as Image
import           System.FilePath (dropFileName, (<.>), (</>))
import           Text.Read (readMaybe)
import qualified SDL.Mixer as Mixer
import Control.DeepSeq (force)


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
    , wt_origin = 0
    }

setGroundOrigin :: WrappedTexture -> WrappedTexture
setGroundOrigin wt =
  let V2 x y = wt_size wt
   in wt
        { wt_origin = V2 (x `div` 2) y
        }


soundName :: Sound -> String
soundName Hit = "hit"
soundName MCSay = "mc"
soundName MarthaSay = "martha"

soundPath :: Sound -> FilePath
soundPath s = "./resources/sounds" </> soundName s <.> "wav"


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
fieldName Another = "another"
fieldName City = "city"

textureName :: GameTexture -> String
textureName Darkness = "darkness"

texturePath :: GameTexture -> String
texturePath t = "./resources/textures" </> textureName t <.> "png"


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

getString :: Value -> Maybe String
getString (String txt) = Just $ T.unpack txt
getString Null = Just ""
getString _ = Nothing


-- TODO(sandy): Super partial function. Sorry. But the tiled datastructure is
-- fucking insane.
parseTilemap :: HasCallStack => Engine -> FieldName -> Tiledmap -> IO Field
parseTilemap e f ti = do
  let renderer = e_renderer e
      force_dir = fromMaybe 0
                $ (readMaybe <=< (getString . propertyValue) <=< M.lookup "force")
                $ tiledmapProperties ti

  tilesets <-
    for (tiledmapTilesets ti) $ \ts ->
      fmap (ts, ) . Image.loadTexture renderer
                  $ dropFileName (fieldPath f) <> tilesetImage ts

  let (ts, tx) = tilesets V.! 0
      tilesize = fmap fromIntegral
          $ V2 (tilesetTilewidth ts) (tilesetTileheight ts)

  pure $
    let res =
          flip foldMap (tiledmapLayers ti) $ \layer ->
            let objs = join $ fmap V.toList $ maybeToList $ layerObjects layer in
            Field
              { f_data = \x y ->
                  case ( within x 0 (tiledmapWidth ti) && within y 0 (tiledmapHeight ti)
                      , layerData layer
                      ) of
                    (True, Just tiledata) -> do
                      let idx = y * tiledmapWidth ti + x
                      case globalToLocal ts $ tiledata V.! idx of
                        Just (LocalId tile) -> do
                          let ix = tile `mod` tilesetColumns ts
                              iy = tile `div` tilesetColumns ts
                          pure $ WrappedTexture
                            { getTexture = tx
                            , wt_sourceRect = Just
                                            $ Rectangle (P $ (* tilesize) $ fmap fromIntegral $ V2 ix iy)
                                            $ tilesize
                            , wt_size = tilesize
                            , wt_origin = 0
                            }
                        Nothing -> mempty
                    _ -> mempty
              , f_size = V2 (layerWidth layer) (layerHeight layer) * fmap fromIntegral tilesize
              , f_tilesize = fmap fromIntegral tilesize
              , f_walkable = \(V2 x y) ->
                  case ( within x 0 (tiledmapWidth ti) && within y 0 (tiledmapHeight ti)
                      , layerData layer
                      , layerObjects layer
                      ) of
                    (True, Just tiledata, _) -> do
                      let idx = y * tiledmapWidth ti + x
                      case globalToLocal ts $ tiledata V.! idx of
                        Just lid -> do
                          let props = tilesetTiles ts M.! lid
                          maybe True (fromBool True . propertyValue) $ M.lookup "walkable" $ tileProperties props
                        Nothing -> True
                    -- Object layers don't obstruct walkability
                    (_, _, Just _) -> True
                    _ -> False
              , f_zones = mapMaybe parseZone objs
              , f_force = 0
              , f_actors = mapMaybe parseActors objs
              , f_blockers = parseBlockers =<< objs
              }
    in res { f_force = force_dir }

close :: [a] -> [a]
close as = take (length as + 1) $ cycle as

parseBlockers :: Object -> [(V2 Double, V2 Double)]
parseBlockers o = case fmap V.toList (objectPolyline o) <> fmap (close . V.toList) (objectPolygon o) of
  Nothing -> mempty
  Just vec ->
    let ps = fmap (V2 (objectX o) (objectY o) +) $ fmap (uncurry V2) $ vec
     in zip ps $ tail ps


parseActors :: Object -> Maybe Actor
parseActors o =
  Actor
    <$> readMaybe (T.unpack $ objectName o)
    <*> pure (V2 (objectX o) (objectY o))
    <*> (parseProperty (objectProperties o) "facing" <|> pure FacingRight)
    <*> pure (parseProperty (objectProperties o) "interaction")

parseProperty :: Read a => Map Text Property -> Text -> Maybe a
parseProperty props name = M.lookup name props >>= getString . propertyValue >>= readMaybe


parseZone :: Object -> Maybe Zone
parseZone o =
  Zone
    <$> readMaybe (T.unpack $ objectType o)
    <*> pure
          (Rectangle
            (P $ V2 (objectX o) (objectY o))
            (V2 (objectWidth o) (objectHeight o)))

fromBool :: Bool -> Value -> Bool
fromBool _ (Bool b) = b
fromBool b Null = b
fromBool _ _ = error "insane value for fromBool. broken tileset"

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

  chars <- fmap (force . M.fromList . join) $
    for [minBound @CharName .. maxBound] $ \char ->
      for [minBound @Anim .. maxBound] $ \anim ->  do
        frames <- for [0 .. frameCounts char anim - 1] $ \i -> do
          let fp = framePath char anim i
          fmap setGroundOrigin . wrapTexture =<< Image.loadTexture renderer fp
        pure ((char, anim), frames)

  fields <- fmap (force . M.fromList) $
    for [minBound @FieldName .. maxBound] $ \fn -> do
      let fp = fieldPath fn
      loadTiledmap fp >>= \case
         Left s -> error s
         Right tm -> fmap (fn, ) $ parseTilemap e fn tm

  textures <- fmap M.fromList $
    for [minBound @GameTexture .. maxBound] $ \tx-> do
      let fp = texturePath tx
      fmap (tx, ) $ wrapTexture =<< Image.loadTexture renderer fp

  sounds <- fmap (M.fromList) $
    for [minBound @Sound .. maxBound] $ \tx-> do
      let fp = soundPath tx
      fmap (tx, ) $ Mixer.load fp


  pure $ Resources
    { r_engine = e
    , r_font = flip M.lookup glyphs
    , r_sprites = curry $ fromMaybe [] . flip M.lookup chars
    , r_fields = fromMaybe (error "missing field data")
               . flip M.lookup fields
    , r_textures = fromMaybe (error "missing texture")
                 . flip M.lookup textures
    , r_sounds = fromMaybe (error "missing sound")
                 . flip M.lookup sounds
    }


