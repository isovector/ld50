{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DerivingStrategies #-}

module Types
  ( module Types
  , module FRP.Yampa
  , Word8
  ) where

import SDL hiding (Event)
import FRP.Yampa
import Control.Monad.Cont
import Control.Monad.Reader
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
  }

type Renderable = Resources -> IO ()

newtype Embedding a b d e = Embedding
  { getEmbedding :: forall c . SF a (b, Event c) -> SF d (e, Event c)
  }
  deriving (Functor)

compEmbed :: Embedding a' b' d e -> Embedding a b a' b' -> Embedding a b d e
compEmbed (Embedding g) (Embedding f) = Embedding (g . f)

embedArr :: (b -> e) -> Embedding d b d e
embedArr f = Embedding (>>> first (arr f))

embedInpArr :: (d -> a) -> Embedding a e d e
embedInpArr f = Embedding (arr f >>>)


newtype Swont i o a = Swont
  { runSwont' :: Cont (SF i o) a
  }
  deriving newtype (Functor, Applicative, Monad)

runSwont :: Swont i o a -> (a -> SF i o) -> SF i o
runSwont x final = runCont (runSwont' x) final

over :: Time -> SF a b -> ReaderT (Embedding a b d e) (Swont d e) ()
over interval sf = do
  Embedding embed' <- ask
  lift . swont $ embed' $ sf &&& (after interval () >>> iPre NoEvent)

swont :: SF a (b, Event c) -> Swont a b c
swont = Swont . cont . switch

