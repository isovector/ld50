{-# LANGUAGE StrictData         #-}

module Types
  ( module Types
  , module FRP.Yampa
  , Word8
  ) where

import Data.Generics.Labels ()
import Control.Monad.Cont
import Data.Word
import FRP.Yampa
import Foreign.C (CInt)
import SDL hiding (Event)
import Data.Coerce (coerce)
import Data.Monoid (All(..))
import Data.Semigroup (Max(..))
import Control.Monad.Reader
import GHC.Generics (Generic)


data Engine = Engine
  { e_renderer :: Renderer
  }
  deriving Generic


data Controls = Controls
  { c_action :: Bool
  , c_restart :: Bool
  , c_arrows :: V2 Int
  }
  deriving Generic


defaultControls :: Controls
defaultControls = Controls False False $ pure 0

data CharName
  = MainCharacter
  | Martha
  | Claptrap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Anim
  = Idle
  | NoAnim
  | Run
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data FieldName
  = TestField
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GameTexture
  = Darkness
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Resources = Resources
  { r_engine :: Engine
  , r_font :: Char -> Maybe Texture
  , r_sprites :: CharName -> Anim -> [WrappedTexture]
  , r_fields :: FieldName -> Field
  , r_textures :: GameTexture -> WrappedTexture
  }
  deriving Generic

data Field = Field
  { f_data :: Int -> Int -> [WrappedTexture]
  , f_tilesize :: V2 Double
  , f_walkable :: V2 Int -> Bool
  , f_zones :: [Zone]
  }
  deriving Generic

data ZoneType
  = SendMessage Message
  deriving (Eq, Ord, Show, Read)

data Zone = Zone
  { z_type :: ZoneType
  , z_rect :: Rectangle Double
  }
  deriving (Generic, Show)


instance Semigroup Field where
  Field f1 v1 p1 z1 <> Field f2 v2 p2 z2
    = Field (f1 <> f2)
            (coerce ((<>) @(Max (V2 Double))) v1 v2)
            (coerce ((<>) @(V2 Int -> All)) p1 p2)
            (z1 <> z2)

instance Monoid Field where
  mempty = Field mempty 0 (const True) mempty



data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  }
  deriving Generic

type Renderable = Resources -> IO ()


data WrappedTexture = WrappedTexture
  { getTexture    :: Texture
  , wt_sourceRect :: Maybe (Rectangle CInt)
  , wt_size       :: V2 CInt
  , wt_origin     :: V2 CInt
  }
  deriving Generic


--------------------------------------------------------------------------------
-- | An embedding is an SF transformation from (a -> b) to (d -> e), such that
-- they also carry along an @Event c@. Why would you want to do that? So that
-- you can embed one SF to run inside of another, eg, if you are building
-- a state machine via @Swont@.
newtype Embedding a b d e = Embedding
  { getEmbedding :: SF a b -> SF d e
  }
  deriving (Functor)

type Embedding' i o = Embedding i o i o


newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

data FSM i o = FSM
  { fsm_embedding :: Embedding i o i o
  , fsm_root      :: Bool
  }

------------------------------------------------------------------------------
-- |
newtype Compositing d e i o a = Compositing
  { getCompositing :: ReaderT (FSM d e) (Swont i o) a
  } deriving (Functor, Applicative, Monad, MonadReader (FSM d e))

type Compositing' i o = Compositing i o i o


data Message
  = Ok
  | Restart
  | Quit
  | HitWall
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

