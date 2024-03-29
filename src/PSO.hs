module PSO where

import           Protolude
import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector         as VB
import qualified Data.Vector.Unboxed as VU
import           System.Random.MWC.Monad (Rand(..))
import qualified System.Random.MWC.Monad as RM
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Primitive.Class (MonadPrim)

import           Control.Newtype (Newtype(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Evaluator(..), Dim(..), BoundsVector(..), Bounds(..))
import qualified Functions as F


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

data Env = Env
    { config       :: Config
    , costFunction :: CostFunction
    }

type PrimMonadPrim m = (PrimMonad m, MonadPrim m)

type Comp m a = ReaderT Env (StateT Integer (Rand m)) a

liftRand :: (MonadPrim m) => Rand m a -> Comp m a
liftRand = lift . lift

-- Lenses

particlePos, bestParticlePos, bestLocalPos :: Particle -> Position
particlePos     = position . particleEvalPos
bestParticlePos = position . bestParticleEvalPos
bestLocalPos    = position . bestLocalEvalPos

-- Generators

genVectorFor :: PrimMonadPrim m => (VU.Vector Double -> a) -> BoundsVector -> Rand m a
genVectorFor cons (BoundsVector boundsVector) = do
    vecB <- forM boundsVector createVec
    return . cons $ VG.convert vecB
  where
    createVec (Bounds l u) = RM.uniformR (l, u)

genPosition :: PrimMonadPrim m => BoundsVector -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: PrimMonadPrim m => BoundsVector -> Rand m Velocity
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

genParticle :: PrimMonadPrim m => BoundsVector -> EvaluatedPosition -> EvaluatedPosition -> Rand m Particle
genParticle boundsVector bestLocalEvalPosition evalPos = do
    vel <- genVelocity $ genVelocityBounds boundsVector
    return $ Particle vel evalPos evalPos bestLocalEvalPosition

genEvaluatedPosition :: PrimMonadPrim m => Comp m EvaluatedPosition
genEvaluatedPosition = do
    costFun <- asks costFunction
    pos <- liftRand $ genPosition $ F.boundsVector costFun
    return $ calculateEvalPos (F.evaluator costFun) pos

genSwarm :: PrimMonadPrim m => Comp m Swarm
genSwarm = do
    swarmSize <- asks $ Config.swarmSize . config
    costFun   <- asks costFunction
    evaluatedPositions <- VG.replicateM swarmSize genEvaluatedPosition
    let bestLocalEvalPosition = VG.minimumBy (comparing value) evaluatedPositions
    fmap Swarm $ liftRand $ forM evaluatedPositions $ genParticle (F.boundsVector costFun) bestLocalEvalPosition

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

genRandomVector :: PrimMonadPrim m => Dim -> Rand m RandVector
genRandomVector (Dim d) = fmap RandVector $ VU.replicateM d $ RM.uniformR rRange

updateParticlePosVel :: PrimMonadPrim m => Dim -> Evaluator -> Particle -> Rand m Particle
updateParticlePosVel dim evaluator particle = do
    rPV <- genRandomVector dim
    rLV <- genRandomVector dim
    return $ updateParticleVelPosEval evaluator rPV rLV particle

updateParticlesPosVel :: (Vector v Particle, PrimMonadPrim m) => v Particle -> Comp m (v Particle)
updateParticlesPosVel particles = do
    costFun <- asks costFunction
    let dim = F.dimension costFun
        evaluator = F.evaluator costFun
    liftRand $ VG.forM particles $ updateParticlePosVel dim evaluator

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

updateSwarm :: PrimMonadPrim m => Swarm -> Comp m Swarm
updateSwarm (Swarm particles) = do
    newParticles <- updateParticlesPosVel particles
    let maybeNewBestLocalEvalPos = calculateBestLocalEvalPos newParticles
        newParticlesWithBestLocal = maybeUpdateBestLocal maybeNewBestLocalEvalPos newParticles
    return $ pack newParticlesWithBestLocal

getBest :: Swarm -> Particle
getBest swarm = unpack swarm & VG.minimumBy (comparing $ value . bestLocalEvalPos)

optimize :: (MonadIO m, PrimMonadPrim m) => Integer -> Swarm -> Comp m (Particle, Swarm)
optimize epoch swarm = do
    maxEpochs <- asks $ Config.epochsDef    . config
    logEpochs <- asks $ Config.logEpochsDef . config
    newSwarm <- updateSwarm swarm
    when (epoch `mod` logEpochs == 0) $ do
        let best = value . bestLocalEvalPos $ getBest newSwarm
        liftIO $ putText $ "Epoch: " <> show epoch <> " best: " <> show best
    -- putText $ "New swarm:\n" <> ppShow newSwarm
    if epoch < maxEpochs
        then optimize (epoch + 1) newSwarm
        else return (getBest newSwarm, newSwarm)

-- Tests

runComp :: PrimMonadPrim m => RM.Seed -> Env -> Integer -> Comp m a -> m a
runComp seed cfg st comp = RM.runWithSeed seed $ evalStateT (runReaderT comp cfg) st

computation :: (PrimMonadPrim m, MonadIO m) =>  Comp m (Particle, Swarm)
computation = do
    initialSwarm <- genSwarm
    -- liftIO $ putText $ "Initial swarm:\n" <> ppShow initialSwarm
    optimize 0 initialSwarm

test :: Config -> IO ()
test cfg = do
    seedNum <- Config.seedDef cfg
    let genSeed = RM.toSeed $ VU.singleton seedNum
        costFun = F.rastrigin $ Config.dimension cfg
    (result, _finalSwarm) <- runComp genSeed (Env cfg costFun) 0 computation
    -- putText $ "Result:\n" <> (renderStyle mystyle $ ppDoc result)
    -- putText $ "Final swarm:\n" <> ppShow finalSwarm
    putText $ "Best result:\n" <> ppShow (value $ bestLocalEvalPos result)
    putText $ "Seed: " <> ppShow seedNum
    -- putText $ "Cost function bounds:\n" <> ppShow (F.boundsVector costFun)
