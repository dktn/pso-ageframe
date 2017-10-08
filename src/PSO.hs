module PSO where

import           Protolude
import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector         as VB
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

newtype Swarm = Swarm (VB.Vector Particle)
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
    return . cons $ VG.convert vecB
  where
    createVec (Bounds l u) = RM.uniformR (l, u)

genPosition :: RandMonad m => BoundsVector -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: RandMonad m => BoundsVector -> Rand m Velocity
genVelocity = genVectorFor Velocity

calculateEvalPos :: Evaluator -> Position -> EvaluatedPosition
calculateEvalPos evaluator pos = EvaluatedPosition pos $ evalCost evaluator pos
  where
    evalCost :: Evaluator -> Position -> Value
    evalCost (Evaluator e) (Position p) = Value $ e p

genVelocityBounds :: BoundsVector -> BoundsVector
genVelocityBounds (BoundsVector bounds) = BoundsVector $ toVelocityBound <$> bounds
  where
    toVelocityBound (Bounds l u) = let range = abs $ u - l in Bounds (-range) range

genParticle :: RandMonad m => BoundsVector -> EvaluatedPosition -> EvaluatedPosition -> Rand m Particle
genParticle boundsVector bestLocalEvalPosition evalPos = do
    vel <- genVelocity $ genVelocityBounds boundsVector
    return $ Particle vel evalPos evalPos bestLocalEvalPosition

genEvaluatedPosition :: RandMonad m => CostFunction -> Rand m EvaluatedPosition
genEvaluatedPosition costFunction = do
    pos <- genPosition $ F.boundsVector costFunction
    return $ calculateEvalPos (F.evaluator costFunction) pos

genSwarm :: RandMonad m => Int -> CostFunction -> Rand m Swarm
genSwarm swarmSize costFunction = do
    evaluatedPositions <- VG.replicateM swarmSize $ genEvaluatedPosition costFunction
    let bestLocalEvalPosition = VG.minimumBy (comparing value) evaluatedPositions
    fmap Swarm . forM evaluatedPositions $ genParticle (F.boundsVector costFunction) bestLocalEvalPosition

-- Optimization

data Params a = Params
    { paramOmega :: a
    , paramPhiP  :: a
    , paramPhiL  :: a
    } deriving (Show)

globalParams :: Params Double
globalParams = Params { paramOmega = 0.729, paramPhiP = 1.49445, paramPhiL = 1.49445 }

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

    applyVelocity :: (Vector v a, Num a) => v a -> v a -> v a
    applyVelocity = VG.zipWith applyVelocityI

    applyVelocityI :: Num a => a -> a -> a
    applyVelocityI vel x = x + vel

    newParticleVelocity :: Velocity
    newParticleVelocity = pack $ calcVelocity globalParams
                                              (unpack rPV)
                                              (unpack rLV)
                                              (unpack $ particleVelocity particle)
                                              (unpack $ particlePos particle)
                                              (unpack $ bestParticlePos particle)
                                              (unpack $ bestLocalPos particle)

    calcVelocity :: (Vector v a, Num a) => Params a -> v a -> v a -> v a -> v a -> v a -> v a -> v a
    calcVelocity params = VG.zipWith6 $ calcVelocityI params

    calcVelocityI :: Num a => Params a -> a -> a -> a -> a -> a -> a -> a
    calcVelocityI (Params omega phiP phiL) rP rL v pos bestPPos bestLPos = omega * v + rP * phiP * (bestPPos - pos) + rL * phiL * (bestLPos - pos)
    -- v ← ωv + φp rp (p-x) + φg rg (g-x)

updateParticlePosVel :: RandMonad m => CostFunction -> Particle -> Rand m Particle
updateParticlePosVel costFunction particle = do
    let dim = unpack $ F.dimension costFunction
    rPV <- fmap RandVector $ VU.replicateM dim $ RM.uniformR rRange
    rLV <- fmap RandVector $ VU.replicateM dim $ RM.uniformR rRange
    let newParticle = updateParticleVelPosEval (F.evaluator costFunction) rPV rLV particle
    return newParticle

updateParticlesPosVel :: (Vector v Particle, RandMonad m) => CostFunction -> v Particle -> Rand m (v Particle)
updateParticlesPosVel costFunction particles = VG.forM particles $ updateParticlePosVel costFunction

calculateBestLocalEvalPos :: Vector v Particle => v Particle -> EvaluatedPosition
calculateBestLocalEvalPos particles = bestParticleEvalPos $ VG.minimumBy (comparing $ value . bestParticleEvalPos) particles

maybeUpdateBestLocal :: Vector v Particle => EvaluatedPosition -> v Particle -> v Particle
maybeUpdateBestLocal maybeNewBestLocalEvalPos = VG.map maybeUpdateBestLocalParticle
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
getBest swarm = unpack swarm & VG.minimumBy (comparing $ value . bestLocalEvalPos)

optimize :: (MonadIO m, RandMonad m) => Integer -> Integer -> CostFunction -> Swarm -> Rand m (Particle, Swarm)
optimize epoch maxEpochs costFunction swarm = do
    newSwarm <- updateSwarm costFunction swarm
    when (epoch `mod` 20 == 0) $ do
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
        epochs = Config.epochsDef cfg
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
