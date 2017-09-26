module PSO where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import           System.Random.MWC as R
import           System.Random.MWC.Monad (Rand(..))
import           System.Random.MWC.Monad as RM
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Primitive.Class (MonadPrim)
import           GHC.Word (Word32)

import           Config (Config(..))

-- explicit gen passing

createGen :: PrimMonad m => Word32 -> m (Gen (PrimState m))
createGen = R.initialize . VU.singleton

genPopulation :: PrimMonad m => Gen (PrimState m) -> Int -> Double -> Double -> m (Vector Double)
genPopulation gen n from to = VU.replicateM n $ R.uniformR (from, to) gen

test1 :: Config -> IO ()
test1 cfg = do
    gen <- createGen $ seed cfg
    pop <- genPopulation gen (population cfg) (-5.12) 5.12
    print pop
    return ()

-- implicit gen passing

genPopulation2 :: (PrimMonad m, MonadPrim m) => Int -> Double -> Double -> Rand m (Vector Double)
genPopulation2 n from to = VU.replicateM n $ RM.uniformR (from, to)

test2 :: Config -> IO ()
test2 cfg = do
    -- let genSeed = RM.toSeed $ VU.singleton $ seed cfg
    -- pop <- runWithSeed genSeed $ do
    pop <- flip RM.runWithVector (VU.singleton $ seed cfg) $ do
        genPopulation2 (population cfg) (-5.12) 5.12
    print pop
