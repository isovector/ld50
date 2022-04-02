{-# LANGUAGE DerivingStrategies #-}

module Types
  ( module Types
  , module FRP.Yampa
  , Word8
  ) where

import SDL hiding (Event)
import FRP.Yampa
import Control.Monad.Cont
import Data.Word


data Engine = Engine
  { e_renderer :: Renderer
  }


data Controls = Controls
  { c_action :: Bool
  }


defaultControls :: Controls
defaultControls = Controls False


data Resources = Resources
  { r_engine :: Engine
  , r_font :: Char -> Maybe Texture
  }


type Renderable = Resources -> IO ()


newtype Embedding a b d e = Embedding
  { getEmbedding :: forall c . SF a (b, Event c) -> SF d (e, Event c)
  }
  deriving (Functor)


newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

