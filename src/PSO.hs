module PSO where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
-- import qualified System.Random.MWC as R
import           System.Random.MWC.Monad (Rand(..))
import qualified System.Random.MWC.Monad as RM
-- import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Primitive.Class (MonadPrim)
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

import           Config (Config())
import qualified Config



genPopulationM :: (PrimMonad m, MonadPrim m) => Int -> Double -> Double -> Rand m (Vector Double)
genPopulationM n from to = VU.replicateM n $ RM.uniformR (from, to)

optimize :: (PrimMonad m, MonadPrim m) => Vector Double -> Integer -> Rand m (Vector Double)
optimize population iterations = do
    return population

test :: Config -> IO ()
test cfg = do
    let genSeed = RM.toSeed . VU.singleton $ Config.seed cfg
    result <- RM.runWithSeed genSeed $ do
        initialPopulation <- genPopulationM (Config.population cfg) (-5.12) 5.12
        liftIO $ putStrLn $ "Initial population:\n" <> show initialPopulation
        optimize initialPopulation $ fromMaybe 100 $ Config.iterations cfg
    putStrLn $ "Result:\n" <> show result
