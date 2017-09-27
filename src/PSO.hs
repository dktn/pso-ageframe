module PSO where

import qualified Data.Vector as V
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
import qualified Functions

newtype Position = Position
    { unPos :: VU.Vector Double
    } deriving (Show)

newtype Velocity = Velocity
    { unVel :: VU.Vector Double
    } deriving (Show)

newtype Value = Value
    { unVal :: Double
    } deriving (Show)

data Particle = Particle
    { position           :: Position
    , velocity           :: Velocity
    , bestPosition       :: Position
    , bestValue          :: Value
    , bestGlobalPosition :: Position
    , bestGlobalValue    :: Value
    } deriving (Show)

newtype Swarm = Swarm
    { particles :: V.Vector Particle
    } deriving (Show)

genParticle :: (PrimMonad m, MonadPrim m) => Int -> Double -> Double -> Rand m Particle
genParticle dim from to = do
    position <- fmap Position $ VU.replicateM dim $ RM.uniformR (from, to)
    velocity <- fmap Velocity $ VU.replicateM dim $ RM.uniformR (from, to)
    let bestValue = Value 10000.0
    return $ Particle position velocity position bestValue position bestValue

genSwarm :: (PrimMonad m, MonadPrim m) => Int -> Int -> Double -> Double -> Rand m Swarm
genSwarm n dim from to = fmap Swarm $ V.replicateM n $ genParticle dim from to

optimize :: (PrimMonad m, MonadPrim m) => Swarm -> Integer -> Rand m Particle
optimize swarm iterations = do
    return $ V.head $ particles swarm

test :: Config -> IO ()
test cfg = do
    let genSeed = RM.toSeed . VU.singleton $ Config.seed cfg
    result <- RM.runWithSeed genSeed $ do
        initialSwarm <- genSwarm (Config.swarmSize cfg) (Config.dimension cfg) (-5.12) 5.12
        liftIO $ putStrLn $ "Initial swarm:\n" <> show initialSwarm
        optimize initialSwarm $ fromMaybe 100 $ Config.iterations cfg
    putStrLn $ "Result:\n" <> show result
