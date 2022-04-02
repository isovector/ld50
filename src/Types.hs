{-# LANGUAGE DerivingStrategies #-}

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
  }


defaultControls :: Controls
defaultControls = Controls False

data Character
  = MainCharacter
  | Martha
  | Claptrap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Anim
  = Idle
  | NoAnim
  | Run
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data Resources = Resources
  { r_engine :: Engine
  , r_font :: Char -> Maybe Texture
  , r_sprites :: Character -> Anim -> [WrappedTexture]
  }


type Renderable = Resources -> IO ()


data WrappedTexture = WrappedTexture
  { getTexture :: Texture
  , wt_size    :: V2 CInt
  }


newtype Embedding a b d e = Embedding
  { getEmbedding :: forall c . SF a (b, Event c) -> SF d (e, Event c)
  }
  deriving (Functor)


newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

