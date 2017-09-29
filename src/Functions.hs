module Functions where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM

data Bounds = Bounds Double Double
newtype Dim = Dim Int

data CostFunction = CostFunction
    { dim    :: Dim
    , cost   :: Vector Double -> Double
    , bounds :: Bounds
    }

rastrigin :: Dim -> CostFunction
rastrigin d = CostFunction d rastriginFunction (Bounds (-5.12) 5.12)

{-# INLINE rastrigin #-}
rastriginFunction :: Vector Double -> Double
rastriginFunction vect = a * n + sum1N
  where
    n     = fromIntegral $ VU.length vect
    a     = 10.0
    pi2   = 2.0 * pi
    sum1N = VU.foldl' (\acc x -> acc + (x * x - a * cos(pi2 * x))) 0.0 vect
