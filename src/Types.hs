{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types
  ( module Types
  , module FRP.Yampa
  , module Debug.Trace
  , module Control.Applicative
  , (<&>)
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
import Debug.Trace (traceShowId, traceM)
import Data.Functor ((<&>))
import Control.Applicative
import SDL.Mixer (Chunk)
import Graphics.Rendering.OpenGL (Program)
import Graphics.Rendering.OpenGL.GL (UniformLocation)
import Control.DeepSeq

deriving anyclass instance NFData a => NFData (Rectangle a)


data Engine = Engine
  { e_renderer :: Renderer
  , e_window :: Window
  , e_buffer :: Texture
  , e_shadows :: Texture
  , e_shader_program :: Program
  , e_uniform_locs :: Uniforms (Const UniformLocation)
  }
  deriving stock Generic


data Controls = Controls
  { c_action :: Bool
  , c_restart :: Bool
  , c_arrows :: V2 Int
  }
  deriving stock Generic
  deriving anyclass (NFData)


defaultControls :: Controls
defaultControls = Controls False False $ pure 0

data CharName
  = MainCharacter
  | Martha
  | Claptrap
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data Anim
  = Idle
  | NoAnim
  | Run
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data FieldName
  = TestField
  | Another
  | City
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data GameTexture
  = Darkness
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data Resources = Resources
  { r_engine :: Engine
  , r_font :: Char -> Maybe Texture
  , r_sprites :: CharName -> Anim -> [WrappedTexture]
  , r_fields :: FieldName -> Field
  , r_textures :: GameTexture -> WrappedTexture
  , r_sounds :: Sound -> Chunk
  }
  deriving stock Generic
  -- deriving anyclass (NFData)

data Field = Field
  { f_data :: Int -> Int -> [WrappedTexture]
  , f_size :: V2 Double
  , f_tilesize :: V2 Double
  , f_walkable :: V2 Int -> Bool
  , f_zones :: [Zone]
  , f_force :: V2 Int
  , f_actors :: [Actor]
  , f_blockers :: [(V2 Double, V2 Double)]
  }
  deriving stock Generic
  deriving anyclass (NFData)

data Facing = FacingLeft | FacingRight
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data Sound
  = Hit
  | MCSay
  | MarthaSay
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data Actor = Actor
  { a_name        :: CharName
  , a_pos         :: V2 Double
  , a_facing      :: Facing
  , a_interaction :: Maybe ActorInteraction
  }
  deriving stock (Show, Generic)
  deriving anyclass NFData

data ZoneType
  = SendMessage WorldInteraction
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass NFData

data Zone = Zone
  { z_type :: ZoneType
  , z_rect :: Rectangle Double
  }
  deriving stock (Generic, Show)
  deriving anyclass NFData


instance Semigroup Field where
  Field f1 s1 v1 p1 z1 _ a1 b1 <> Field f2 s2 v2 p2 z2 _ a2 b2
    = Field (f1 <> f2)
            (coerce ((<>) @(Max (V2 Double))) s1 s2)
            (coerce ((<>) @(Max (V2 Double))) v1 v2)
            (coerce ((<>) @(V2 Int -> All)) p1 p2)
            (z1 <> z2)
            0
            (a1 <> a2)
            (b1 <> b2)

instance Monoid Field where
  mempty = Field mempty 0 0 (const True) mempty 0 mempty mempty



data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  }
  deriving stock Generic
  deriving anyclass NFData

type Renderable = Resources -> IO ()


data WrappedTexture = WrappedTexture
  { getTexture    :: Texture
  , wt_sourceRect :: Maybe (Rectangle CInt)
  , wt_size       :: V2 CInt
  , wt_origin     :: V2 CInt
  }
  deriving stock Generic

instance NFData WrappedTexture where
  rnf (WrappedTexture !tex m_rec v2 v2') = m_rec `seq` v2 `seq` v2' `seq` ()


--------------------------------------------------------------------------------
-- | An embedding is an SF transformation from (a -> b) to (d -> e), such that
-- they also carry along an @Event c@. Why would you want to do that? So that
-- you can embed one SF to run inside of another, eg, if you are building
-- a state machine via @Swont@.
newtype Embedding a b d e = Embedding
  { getEmbedding :: SF a b -> SF d e
  }
  deriving stock (Functor, Generic)
  deriving anyclass NFData

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
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader (FSM d e))
  deriving stock Generic

type Compositing' i o = Compositing i o i o


data Message
  = Interact
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass NFData

data WorldInteraction
  = Goto FieldName (V2 Double)
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass NFData

data ActorInteraction
  = PortalWarning
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass NFData

data Uniforms f = Uniforms
  { u_mood_color :: f (V4 Float)
  }

deriving instance Show (f (V4 Float)) => Show (Uniforms f)


data Switch i o a
  = Push (a -> Compositing' i o (a, Switch i o a))
  | Bind (a -> Compositing' i o (a, Switch i o a))
  | Done a

instance (Eq a,Floating a) => VectorSpace (V2 a) a where
  zeroVector = 0
  (*^) a = fmap (* a)
  (^+^) = (+)
  dot v1 v2 = sum $ liftA2 (*) v1 v2

