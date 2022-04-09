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
import System.FilePath ((</>))
import Graphics.Rendering.OpenGL (Color4(Color4), GLfloat)
import Shaders
import Control.Concurrent (threadDelay)
import SDL.Raw.Video (composeCustomBlendMode, setTextureBlendMode)
import SDL.Raw.Enum
import qualified SDL.Raw.Types
import Unsafe.Coerce (unsafeCoerce)


makeShader :: ShaderType -> FilePath -> IO Shader
makeShader ty fp = do
  shader <- createShader ty
  src <- readFile $ "resources/shaders" </> fp
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
  shadows <- createTexture renderer RGBA8888 TextureAccessTarget screenSize
  bm <- composeCustomBlendMode
          SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR
          SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR
          -- SDL_BLENDFACTOR_DST_COLOR
          SDL_BLENDOPERATION_ADD
          SDL_BLENDFACTOR_SRC_ALPHA
          SDL_BLENDFACTOR_SRC_ALPHA
          SDL_BLENDOPERATION_ADD
  void $ setTextureBlendMode (getRawTexture shadows) bm

  program <- createProgram
  attachShader program =<< makeShader VertexShader   "std.vertex"
  attachShader program =<< makeShader FragmentShader "test.fragment"



  validateProgram program
  linkProgram program
  currentProgram $= Just program

  us <- getUniforms program
  let engine = Engine
        { e_renderer = renderer
        , e_window = window
        , e_buffer = buffer
        , e_shadows = shadows
        , e_shader_program = program
        , e_uniform_locs = us
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


getRawTexture :: Texture -> SDL.Raw.Types.Texture
getRawTexture = unsafeCoerce



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
  -- currentProgram $= Just (e_shader_program e)

  rendererRenderTarget renderer $= Nothing
  glBindTexture $ e_buffer e

  pushUniforms e $ Uniforms (pure $ V4 1 0 0 0.5)

  glSwapWindow $ e_window e
  threadDelay 1
  pure False


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9

