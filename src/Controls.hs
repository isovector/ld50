{-# LANGUAGE Arrows #-}

module Controls where

import Types


waitControls :: SF Controls [()]
waitControls = proc controls -> do
  ok <- edgeTag [()] -< c_action controls
  returnA -< event [] id ok

