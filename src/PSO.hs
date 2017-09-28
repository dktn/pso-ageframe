{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- import           Text.Pretty.Simple (pShow)
-- import           Text.Show.Pretty (ppShow)
import           Text.Show.Pretty (ppDoc)
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))

import           Config (Config())
import qualified Config
import qualified Functions

-- TODO:
-- pretty printing, default vect, initialize globals
-- what velocity, velocity changes, global propagation

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

genPosition :: (RandMonad m) => Int -> Double -> Double -> Rand m Position
genPosition dim from to = fmap Position $ VU.replicateM dim $ RM.uniformR (from, to)

genVelocity :: (RandMonad m) => Int -> Double -> Double -> Rand m Velocity
genVelocity dim from to = fmap Velocity $ VU.replicateM dim $ RM.uniformR (from, to)

genParticle :: (RandMonad m) => Int -> Double -> Double -> Rand m Particle
genParticle dim from to = do
    position <- genPosition dim from to
    velocity <- genVelocity dim (from / 100.0) (to / 100.0)
    let bestValue = Value 10000.0
    return $ Particle position velocity position bestValue position bestValue

genSwarm :: (RandMonad m) => Int -> Int -> Double -> Double -> Rand m Swarm
genSwarm n dim from to = fmap Swarm $ V.replicateM n $ genParticle dim from to

optimize :: (RandMonad m) => Swarm -> Integer -> Rand m Particle
optimize swarm iterations = do
    return $ V.head $ particles swarm

ppShow :: Show a => a -> String
ppShow = renderStyle wideStyle . ppDoc
  where
    wideStyle = Style PageMode 238 1.5

test :: Config -> IO ()
test cfg = do
    let genSeed = RM.toSeed . VU.singleton $ Config.seed cfg
    result <- RM.runWithSeed genSeed $ do
        initialSwarm <- genSwarm (Config.swarmSize cfg) (Config.dimension cfg) (-5.12) 5.12
        liftIO $ putStrLn $ "Initial swarm:\n" <> ppShow initialSwarm
        optimize initialSwarm $ fromMaybe 100 $ Config.iterations cfg
    -- putStrLn $ "Result:\n" <> (renderStyle mystyle $ ppDoc result)
    putStrLn $ "Result:\n" <> ppShow result
    -- putStrLn $  renderStyle mystyle $ ppDoc [1..25]
