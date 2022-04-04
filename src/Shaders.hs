module Shaders
  ( getUniforms
  , pushUniforms
  )
  where

import Types
import Data.Functor.Identity
import Graphics.Rendering.OpenGL.GL.Shaders
import Data.Foldable (traverse_)
import Graphics.Rendering.OpenGL
import SDL (V4 (..))

getUniforms :: Program -> IO (Uniforms (Const UniformLocation))
getUniforms p =
  let getU = fmap Const . uniformLocation p
   in Uniforms <$> getU "mood_color"


v4ToColor4 :: V4 Float -> Color4 Float
v4ToColor4 (V4 x y z w) = Color4 x y z w

zipUniforms :: Uniforms (Const UniformLocation) -> Uniforms Identity -> [UniformValue]
zipUniforms (Uniforms f1) (Uniforms g1) =
  [ UniformValue (getConst f1) $ v4ToColor4 $ runIdentity g1
  ]

data UniformValue where
  UniformValue :: Uniform a => UniformLocation -> a -> UniformValue

pushUniforms :: Engine -> Uniforms Identity -> IO ()
pushUniforms e us = do
  traverse_ (\(UniformValue u v) -> uniform u $= v) $ zipUniforms (e_uniform_locs e) us



