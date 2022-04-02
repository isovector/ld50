module Types
  ( module Types
  , module FRP.Yampa
  ) where

import SDL
import FRP.Yampa

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

