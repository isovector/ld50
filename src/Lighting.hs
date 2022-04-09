module Lighting where

import Overlude
import Data.Maybe (mapMaybe, maybeToList, fromMaybe)
import Data.List (sortBy, sortOn, minimumBy)
import Data.Ord (comparing)


lighting :: Double -> [(V2 Double, V2 Double)] -> V2 Double -> [Triangle (V2 Double)]
lighting dist blockers src = do
  let ps = sortOn (\((subtract src) -> V2 x y) -> atan2 y x) $ do
        b@(p1, p2) <- blockers
        let dif dt v@(V2 x y) =
              let size = norm v
                  theta = atan2 y x
               in V2 (cos (theta + dt) * size) (sin (theta + dt) * size)
            epsilon = 0.0001
            dst1 = 999 *^ (p1 - src)
            dst2 = 999 *^ (p2 - src)
            f dir = alongLine src dir
                  $ fromMaybe 1
                  $ safeMinimum (* norm dir)
                  $ fmap fst
                  $ filter checkOkIntersect
                  $ mapMaybe (lineIntersect' (src, src + dir))
                  $ blockers

        [  f $ dif (-epsilon) dst1
         , f dst1
         , f $ dif epsilon dst1
         , f $ dif (-epsilon) dst2
         , f dst2
         , f $ dif epsilon dst2
         ]
      ps' = cycle ps
  (p1, p2) <- take (length ps) $ zip ps' $ tail ps'
  pure $ Triangle src (p1) (p2)


alongLine :: V2 Double -> V2 Double -> Double -> V2 Double
alongLine src dir v = src + v *^ dir

safeMinimum :: Ord b => (a -> b) -> [a] -> Maybe a
safeMinimum _ [] = Nothing
safeMinimum f as = Just $ minimumBy (comparing f) as



data Triangle a = Triangle
  { t_v1 :: a
  , t_v2 :: a
  , t_v3 :: a
  } deriving Functor

l1 :: (V2 Double, V2 Double)
l1 = (V2 0 0, V2 1 0)

l2 :: (V2 Double, V2 Double)
l2 = (V2 0.5 (-1), V2 0.5 0.5)

linesIntersection :: (V2 Double, V2 Double) -> (V2 Double, V2 Double) -> Maybe (V2 Double)
linesIntersection a@(start, end) b = onLine $ lineIntersect' a b
  where
    rel = start - end
    onLine Nothing       = Nothing
    onLine (Just xy@(x, _))
      | checkOkIntersect xy = Just $ x *^ rel + start
      | otherwise = Nothing

checkOkIntersect :: (Double, Double) -> Bool
checkOkIntersect (x, y) =
  x >= 0 && x <= 1 && y >= 0 && y <= 1


------------------------------------------------------------------------------
-- | Get two scalars describing how far along each line the intersection
-- happens
lineIntersect' :: (V2 Double, V2 Double) -> (V2 Double, V2 Double) -> Maybe (Double, Double)
lineIntersect' (xy@(V2 x y),   xyend)
               (xy'@(V2 x' y'), xy'end)
  | a == 0 = Nothing
  | otherwise = Just (t, t')
  where
    V2 xd yd = xyend - xy
    V2 xd' yd' = xy'end - xy'
    a = (xd' * yd) - (xd * yd')
    t' = ((xd * (y' - y)) - (yd * (x' - x))) / a
    t = ((xd' * (y - y')) - (yd' * (x - x'))) / (negate a)

