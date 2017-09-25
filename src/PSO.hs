module PSO where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import           System.Random.MWC
import           Control.Monad.Primitive (PrimState)
import           GHC.Word (Word32)

import           Config (Config(..))

createGen :: Word32 -> IO (Gen (PrimState IO))
createGen = initialize . VU.singleton

genPopulation :: Gen (PrimState IO) -> Int -> Double -> Double -> IO (Vector Double)
genPopulation gen n from to = VU.replicateM n (uniformR (from, to) gen)

test :: Config -> IO ()
test cfg = do
    gen <- createGen $ seed cfg
    pop <- genPopulation gen 10 (-5.12) 5.12
    print pop
    return ()