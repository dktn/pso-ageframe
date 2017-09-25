module Functions where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM

-- x in -5.12..5.12
rastrigin :: Vector Double -> Double
rastrigin vect = a * n + sum1N
  where
    n     = fromIntegral $ VU.length vect
    a     = 10.0
    pi2   = 2.0 * pi
    sum1N = VU.foldl' (\acc x -> acc + (x * x - a * cos(pi2 * x))) 0.0 vect
