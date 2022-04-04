{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall          #-}

module Main where

import Control.Monad
import Controls (parseControls)
import Data.IORef
import Data.String
import Data.Time.Clock.System
import FRP.Yampa
import Foreign.C.Types
import Game (game)
import Graphics.Rendering.OpenGL.GL.Shaders
import Overlude
import Resources (loadResources)
import SDL hiding (copy, Stereo)
import SDL.Mixer hiding (quit)
import System.Exit



screenScale :: V2 CFloat
screenScale = V2 2 2

physicalScreen :: Num a => V2 a
physicalScreen = V2 160 144

screenSize :: V2 CInt
screenSize = fmap round $ (*) <$> screenScale <*> physicalScreen

makeShader :: ShaderType -> String -> IO Shader
makeShader ty src = do
  shader <- createShader ty
  shaderSourceBS shader $= fromString src
  compileShader shader
  shaderInfoLog shader >>= \case
    "" -> pure ()
    err -> do
      putStrLn err
      exitFailure
  pure shader


main :: IO ()
main = do
  initializeAll

  window <- createWindow "ld50" $ defaultWindow
    { windowInitialSize = screenSize
    , windowGraphicsContext = OpenGLContext defaultOpenGL
    }
  ctx <- glCreateContext window
  glMakeCurrent window ctx
  renderer <- createRenderer window (-1) defaultRenderer
    { rendererType = AcceleratedVSyncRenderer
    , rendererTargetTexture = True
    }
  rendererScale renderer $= screenScale
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  cursorVisible $= False

  openAudio
    (Audio
      { audioFrequency = 44100
      , audioFormat = FormatS16_Sys
      , audioOutput = Stereo
      })
      1024

  buffer <- createTexture renderer RGB888 TextureAccessTarget screenSize

  program <- createProgram
  attachShader program =<<
    makeShader
      VertexShader
      "varying vec4 v_color; varying vec2 v_texCoord; void main() { gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; v_color = gl_Color; v_texCoord = vec2(gl_MultiTexCoord0); }"
  attachShader program =<<
    makeShader
      FragmentShader
      "varying vec2 v_texCoord; varying vec4 v_color; uniform sampler2D tex0; void main() { gl_FragColor = texture2D(tex0, v_texCoord.xy) * vec4(1, 0, 1, 0.5); }"

  validateProgram program
  linkProgram program

  let engine = Engine
        { e_renderer = renderer
        , e_window = window
        , e_buffer = buffer
        , e_shader_program = program
        }
  rs <- loadResources engine

  tS <- getSystemTime
  let seconds = floatSeconds tS
  tRef <- newIORef seconds

  reactimate
    (pure $ FrameInfo defaultControls 0.016)
    (input tRef)
    (output rs)
    (game rs)
  quit



input :: IORef Double -> Bool -> IO (DTime, Maybe FrameInfo)
input tRef _ = do
  pumpEvents
  es <- pollEvents
  when (any (isQuit . eventPayload) es) exitSuccess
  seconds <- readIORef tRef
  tS <- getSystemTime
  let seconds' = floatSeconds tS
  writeIORef tRef seconds'

  keys <- getKeyboardState

  let dt = min 0.016 (seconds' - seconds)
  pure (realToFrac dt, Just $ FrameInfo (parseControls keys) dt)


isQuit :: EventPayload -> Bool
isQuit QuitEvent             = True
isQuit (WindowClosedEvent _) = True
isQuit _                     = False


output :: Resources -> Bool -> Renderable -> IO Bool
output rs _ render = do
  let e = r_engine rs
      renderer = e_renderer e
  rendererDrawColor renderer $= V4 100 149 237 255
  clear renderer
  render rs
  rendererRenderTarget renderer $= Just (e_buffer e)
  currentProgram $= Just (e_shader_program e)

  rendererRenderTarget renderer $= Nothing
  glBindTexture $ e_buffer e
  glSwapWindow $ e_window e
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9

