{-# LANGUAGE Arrows #-}

module Controls where

import Types
import SDL
import Data.Bool (bool)
import Control.Monad


parseControls :: (Scancode -> Bool) -> Controls
parseControls check =
  Controls
    { c_action = check ScancodeZ
    , c_restart = check ScancodeR
    , c_arrows = V2 (arrow ScancodeLeft ScancodeRight)
                    (arrow ScancodeUp ScancodeDown)
    }
  where
    arrow m n = bool (bool 0 1 $ check n) (-1) $ check m


waitControls :: SF Controls [Message]
waitControls = proc controls -> do
  ok <- edgeTag [Ok] -< c_action controls
  restart <- edgeTag [Restart] -< c_restart controls
  returnA -< event [] join $ catEvents [ ok, restart ]

