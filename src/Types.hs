{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module Types
  ( module Types
  , module FRP.Yampa
  , Word8
  ) where

import Control.Monad.Cont
import Data.Word
import FRP.Yampa
import Foreign.C (CInt)
import SDL hiding (Event)


data Engine = Engine
  { e_renderer :: Renderer
  }


data Controls = Controls
  { c_action :: Bool
  , c_restart :: Bool
  , c_arrows :: V2 Int
  }


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


data Resources = Resources
  { r_engine :: Engine
  , r_font :: Char -> Maybe Texture
  , r_sprites :: CharName -> Anim -> [WrappedTexture]
  , r_fields :: FieldName -> Field
  }

data Field = Field
  { f_data :: Int -> Int -> Maybe WrappedTexture
  , f_tilesize :: V2 Double
  , f_static_collision :: Int -> Int -> Bool
  }

data FrameInfo = FrameInfo
  { fi_controls :: Controls
  , fi_dt :: Double
  }

type Renderable = Resources -> IO ()


data WrappedTexture = WrappedTexture
  { getTexture    :: Texture
  , wt_sourceRect :: Maybe (Rectangle CInt)
  , wt_size       :: V2 CInt
  , wt_origin     :: V2 CInt
  }


newtype Embedding a b d e = Embedding
  { getEmbedding :: forall c . SF a (b, Event c) -> SF d (e, Event c)
  }
  deriving (Functor)


newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)


data Message
  = Ok
  | Restart
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

