{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveAnyClass #-}

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
-- import qualified Data.Text.Lazy.IO as L
-- import qualified Data.Text.Lazy as L
-- import           Data.Default (Default(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Bounds(..), Dim(..))
import qualified Functions as F

-- TODO:
-- default vect, initialize globals
-- what velocity, velocity changes, global propagation
-- bounds per dimension

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

type RandMonad m = (PrimMonad m, MonadPrim m)

genVectorIn :: RandMonad m => (VU.Vector Double -> a) -> Dim -> Bounds -> Rand m a
genVectorIn cons (Dim dim) (Bounds from to) = fmap cons $ VU.replicateM dim $ RM.uniformR (from, to)

genPosition :: RandMonad m => Dim -> Bounds -> Rand m Position
genPosition = genVectorIn Position

genVelocity :: RandMonad m => Dim -> Bounds -> Rand m Velocity
genVelocity = genVectorIn Velocity

genParticle :: RandMonad m => CostFunction -> Rand m Particle
genParticle costFunction = do
    let dim = F.dim costFunction
        bounds@(Bounds from to) = F.bounds costFunction
    pos <- genPosition dim bounds
    vel <- genVelocity dim $ Bounds (from / 100.0) (to / 100.0)

    let bestVal = Value 10000.0
    return $ Particle pos vel pos bestVal pos bestVal

genSwarm :: RandMonad m => Int -> CostFunction -> Rand m Swarm
genSwarm swarmSize costFunction = fmap Swarm $ V.replicateM swarmSize $ genParticle costFunction

optimize :: RandMonad m => Swarm -> Integer -> Rand m Particle
optimize swarm iterations = do
    return $ V.head $ particles swarm

test :: Config -> IO ()
test cfg = do
    let genSeed = RM.toSeed . VU.singleton $ Config.seed cfg
        dim = Dim $ Config.dimension cfg
        costFunction = F.rastrigin dim
    result <- RM.runWithSeed genSeed $ do
        initialSwarm <- genSwarm (Config.swarmSize cfg) costFunction
        liftIO $ putStrLn $ "Initial swarm:\n" <> ppShow initialSwarm
        optimize initialSwarm $ fromMaybe 100 $ Config.iterations cfg
    -- putStrLn $ "Result:\n" <> (renderStyle mystyle $ ppDoc result)
    putStrLn $ "Result:\n" <> ppShow result
    -- putStrLn $  renderStyle mystyle $ ppDoc [1..25]
