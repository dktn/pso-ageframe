{-# LANGUAGE RecordWildCards #-}

module PSO where

import           Protolude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
-- import qualified System.Random.MWC as R
import           System.Random.MWC.Monad (Rand(..))
import qualified System.Random.MWC.Monad as RM
-- import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Primitive.Class (MonadPrim)

import           Control.Newtype (Newtype(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Evaluator(..), BoundsVector(..), Bounds(..))
import qualified Functions as F

import qualified Data.Time.Clock as Clock


newtype RandVector = RandVector (VU.Vector Double)
    deriving (Show, Generic, Newtype)

newtype Position = Position (VU.Vector Double)
    deriving (Show, Generic, Newtype)

newtype Velocity = Velocity (VU.Vector Double)
    deriving (Show, Generic, Newtype)

newtype Value = Value Double
    deriving (Show, Eq, Ord, Generic, Newtype)

data EvaluatedPosition = EvaluatedPosition
    { position :: Position
    , value    :: Value
    } deriving (Show)

data Particle = Particle
    { particleVelocity    :: Velocity
    , particleEvalPos     :: EvaluatedPosition
    , bestParticleEvalPos :: EvaluatedPosition
    , bestLocalEvalPos    :: EvaluatedPosition
    } deriving (Show)

newtype Swarm = Swarm (V.Vector Particle)
    deriving (Show, Generic, Newtype)

type RandMonad m = (PrimMonad m, MonadPrim m)

-- Lenses

particlePos, bestParticlePos, bestLocalPos :: Particle -> Position
particlePos     = position . particleEvalPos
bestParticlePos = position . bestParticleEvalPos
bestLocalPos    = position . bestLocalEvalPos

-- Generators

genVectorFor :: RandMonad m => (VU.Vector Double -> a) -> BoundsVector -> Rand m a
genVectorFor cons (BoundsVector boundsVector) = do
    vecB <- forM boundsVector createVec
    return . cons $ V.convert vecB
  where
    createVec (Bounds l u) = RM.uniformR (l, u)

genPosition :: RandMonad m => BoundsVector -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: RandMonad m => BoundsVector -> Rand m Velocity
genVelocity = genVectorFor Velocity

calculateEvalPos :: Evaluator -> Position -> EvaluatedPosition
calculateEvalPos evaluator position = EvaluatedPosition position $ evalCost evaluator position
  where
    evalCost :: Evaluator -> Position -> Value
    evalCost (Evaluator eval) (Position pos) = Value $ eval pos

genVelocityBounds :: BoundsVector -> BoundsVector
genVelocityBounds (BoundsVector bounds) = BoundsVector $ toVelocityBound <$> bounds
  where
    toVelocityBound (Bounds l u) = let range = abs $ u - l in Bounds (-range) range

genParticle :: RandMonad m => BoundsVector -> EvaluatedPosition -> EvaluatedPosition -> Rand m Particle
genParticle boundsVector bestLocalEvalPos evalPos = do
    vel <- genVelocity $ genVelocityBounds boundsVector
    return $ Particle vel evalPos evalPos bestLocalEvalPos

genEvaluatedPosition :: RandMonad m => CostFunction -> Rand m EvaluatedPosition
genEvaluatedPosition costFunction = do
    pos <- genPosition $ F.boundsVector costFunction
    return $ calculateEvalPos (F.evaluator costFunction) pos

genSwarm :: RandMonad m => Int -> CostFunction -> Rand m Swarm
genSwarm swarmSize costFunction = do
    evaluatedPositions <- V.replicateM swarmSize $ genEvaluatedPosition costFunction
    let bestLocalEvalPos = V.minimumBy (comparing value) evaluatedPositions
    fmap Swarm . forM evaluatedPositions $ genParticle (F.boundsVector costFunction) bestLocalEvalPos

-- Optimization
omega :: Double
omega = 0.729

phiP :: Double
phiP = 1.49445

phiL :: Double
phiL = 1.49445

rRange :: (Double, Double)
rRange = (0, 1)

updateParticleVelPosEval :: Evaluator -> RandVector -> RandVector -> Particle -> Particle
updateParticleVelPosEval evaluator rPV rLV particle = particle { particleVelocity    = newParticleVelocity
                                                               , particleEvalPos     = newParticleEvalPos
                                                               , bestParticleEvalPos = newBestParticleEvalPos
                                                               }
  where
    newBestParticleEvalPos :: EvaluatedPosition
    newBestParticleEvalPos = if value newParticleEvalPos < value (bestParticleEvalPos particle)
                                 then newParticleEvalPos
                                 else bestParticleEvalPos particle

    newParticleEvalPos :: EvaluatedPosition
    newParticleEvalPos = calculateEvalPos evaluator newParticlePos

    newParticlePos :: Position
    newParticlePos = pack $ applyVelocity (unpack newParticleVelocity) (unpack $ particlePos particle)

    applyVelocity :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
    applyVelocity = VU.zipWith applyVelocityI

    applyVelocityI :: Double -> Double -> Double
    applyVelocityI vel x = x + vel

    newParticleVelocity :: Velocity
    newParticleVelocity = pack $ calcVelocity (unpack rPV)
                                              (unpack rLV)
                                              (unpack $ particleVelocity particle)
                                              (unpack $ particlePos particle)
                                              (unpack $ bestParticlePos particle)
                                              (unpack $ bestLocalPos particle)

    calcVelocity :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
    calcVelocity = VU.zipWith6 calcVelocityI

    calcVelocityI :: Double -> Double -> Double -> Double -> Double -> Double -> Double
    calcVelocityI rP rL v pos bestPPos bestLPos = omega * v + rP * phiP * (bestPPos - pos) + rL * phiL * (bestLPos - pos)
    -- v ← ωv + φp rp (p-x) + φg rg (g-x)

updateParticlePosVel :: RandMonad m => CostFunction -> Particle -> Rand m Particle
updateParticlePosVel costFunction particle = do
    let dim = unpack $ F.dimension costFunction
    rPV <- fmap RandVector $ VU.replicateM dim $ RM.uniformR rRange
    rLV <- fmap RandVector $ VU.replicateM dim $ RM.uniformR rRange
    let newParticle = updateParticleVelPosEval (F.evaluator costFunction) rPV rLV particle
    return newParticle

updateParticlesPosVel :: RandMonad m => CostFunction -> V.Vector Particle -> Rand m (V.Vector Particle)
updateParticlesPosVel costFunction particles = forM particles $ updateParticlePosVel costFunction

calculateBestLocalEvalPos :: V.Vector Particle -> EvaluatedPosition
calculateBestLocalEvalPos particles = bestParticleEvalPos $ V.minimumBy (comparing $ value . bestParticleEvalPos) particles

maybeUpdateBestLocal :: EvaluatedPosition -> V.Vector Particle -> V.Vector Particle
maybeUpdateBestLocal maybeNewBestLocalEvalPos = fmap maybeUpdateBestLocalParticle
  where
    maybeUpdateBestLocalParticle :: Particle -> Particle
    maybeUpdateBestLocalParticle particle = particle { bestLocalEvalPos = newBestLocalEvalPos }
      where
        newBestLocalEvalPos = if value maybeNewBestLocalEvalPos < value (bestLocalEvalPos particle)
                                  then maybeNewBestLocalEvalPos
                                  else bestLocalEvalPos particle

updateSwarm :: RandMonad m => CostFunction -> Swarm -> Rand m Swarm
updateSwarm costFunction (Swarm particles) = do
    newParticles <- updateParticlesPosVel costFunction particles
    let maybeNewBestLocalEvalPos = calculateBestLocalEvalPos newParticles
        newParticlesWithBestLocal = maybeUpdateBestLocal maybeNewBestLocalEvalPos newParticles
    return $ pack newParticlesWithBestLocal

getBest :: Swarm -> Particle
getBest swarm = unpack swarm & V.minimumBy (comparing $ value . bestLocalEvalPos)

optimize :: (MonadIO m, RandMonad m) => Integer -> Integer -> CostFunction -> Swarm -> Rand m (Particle, Swarm)
optimize epoch maxEpochs costFunction swarm = do
    newSwarm <- updateSwarm costFunction swarm
    when (epoch `mod` 10 == 0) $ do
        let best = value . bestLocalEvalPos $ getBest newSwarm
        liftIO $ putText $ "Epoch: " <> show epoch <> " best: " <> show best
    -- putText $ "New swarm:\n" <> ppShow newSwarm
    if epoch < maxEpochs
        then optimize (epoch + 1) maxEpochs costFunction newSwarm
        else return (getBest newSwarm, newSwarm)

runOptimization :: (MonadIO m, RandMonad m) => Integer -> CostFunction -> Swarm -> Rand m (Particle, Swarm)
runOptimization = optimize 0

-- Tests

getTimeSeed :: IO Word32
getTimeSeed = do
    now <- Clock.getCurrentTime
    putStrLn (show (Clock.utctDay now) ++ "," ++ show (Clock.utctDayTime now))
    -- putStrLn (show (utcToZonedTime utc now :: ZonedTime))
    -- putStrLn (show (utcToZonedTime myzone now :: ZonedTime))
    let timeSeed = fromIntegral . Clock.diffTimeToPicoseconds $ Clock.utctDayTime now
    return timeSeed

test :: Config -> IO ()
test cfg = do
    timeSeed <- getTimeSeed
    let seedNum = fromMaybe timeSeed $ Config.seed cfg
        genSeed = RM.toSeed $ VU.singleton seedNum
        epochs = fromMaybe 100 $ Config.epochs cfg
        dim = Config.dimension cfg
        costFunction = F.rastrigin dim
    (result, finalSwarm) <- RM.runWithSeed genSeed $ do
        initialSwarm <- genSwarm (Config.swarmSize cfg) costFunction
        -- liftIO $ putText $ "Initial swarm:\n" <> ppShow initialSwarm
        runOptimization epochs costFunction initialSwarm
    -- putText $ "Result:\n" <> (renderStyle mystyle $ ppDoc result)
    -- putText $ "Final swarm:\n" <> ppShow finalSwarm
    putText $ "Best result:\n" <> ppShow (value $ bestLocalEvalPos result)
    putText $ "Seed: " <> ppShow seedNum
    -- putText $ "Cost function bounds:\n" <> ppShow (F.boundsVector costFunction)
