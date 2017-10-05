{-# LANGUAGE DeriveAnyClass #-}

module Functions where

import           Protolude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM

import           Control.Newtype (Newtype(..))

data Bounds = Bounds Double Double
    deriving (Show)

newtype BoundsList = BoundsList (V.Vector Bounds)
    deriving (Show, Generic, Newtype)

newtype Dim = Dim Int
    deriving (Show, Generic, Newtype)

newtype Evaluator = Evaluator (VU.Vector Double -> Double)
    deriving (Generic, Newtype)

data CostFunction = CostFunction
    { evaluator  :: Evaluator
    , boundsList :: BoundsList
    }

dimension :: CostFunction -> Dim
dimension costFunction = pack . V.length . unpack $ boundsList costFunction

rastrigin :: Int -> CostFunction
rastrigin d = CostFunction (pack rastriginFunction) bl
  where
    bl = pack $ V.replicate d $ Bounds (-5.12) 5.12

{-# INLINE rastriginFunction #-}
rastriginFunction :: VU.Vector Double -> Double
rastriginFunction vect = a * n + sum1N
  where
    n     = fromIntegral $ VU.length vect
    a     = 10.0
    pi2   = 2.0 * pi
    sum1N = VU.foldl' (\acc x -> acc + (x * x - a * cos(pi2 * x))) 0.0 vect
