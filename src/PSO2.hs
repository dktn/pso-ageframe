module PSO2 where

import           Protolude
import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector         as VB
import qualified Data.Vector.Unboxed as VU
import           System.Random.MWC.Monad (Rand(..))
import qualified System.Random.MWC.Monad as RM
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Primitive.Class (MonadPrim, liftPrim)

import           Control.Newtype (Newtype(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Evaluator(..), Dim(..), BoundsVector(..), Bounds(..))
import qualified Functions as F

import qualified Data.Vector.Fusion.Bundle as Bundle

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

updateParticleVelPosEval :: PrimMonad m => Evaluator -> RandVector -> RandVector -> Particle -> m Particle
updateParticleVelPosEval evaluator rPV rLV particle = do
    newParticleVelocity <- calcNewParticleVelocity
    return $ particle { particleVelocity    = newParticleVelocity
                      , particleEvalPos     = newParticleEvalPos newParticleVelocity
                      , bestParticleEvalPos = newBestParticleEvalPos newParticleVelocity
                      }
  where
    newBestParticleEvalPos :: Velocity -> EvaluatedPosition
    newBestParticleEvalPos newParticleVelocity = if value (newParticleEvalPos newParticleVelocity) < value (bestParticleEvalPos particle)
                                 then newParticleEvalPos newParticleVelocity
                                 else bestParticleEvalPos particle

    newParticleEvalPos :: Velocity -> EvaluatedPosition
    newParticleEvalPos newParticleVelocity = calculateEvalPos evaluator $ newParticlePos newParticleVelocity

    newParticlePos :: Velocity -> Position
    newParticlePos newParticleVelocity = pack $ applyVelocity (unpack newParticleVelocity) (unpack $ particlePos particle)

    applyVelocity :: (Vector v a, Num a) => v a -> v a -> v a
    applyVelocity = VG.zipWith applyVelocityI

    applyVelocityI :: Num a => a -> a -> a
    applyVelocityI vel x = x + vel

    calcNewParticleVelocity :: PrimMonad m => m Velocity
    calcNewParticleVelocity = pack <$> calcVelocity globalParams
                                              (unpack rPV)
                                              (unpack rLV)
                                              (unpack $ bestParticlePos particle)
                                              (unpack $ bestLocalPos particle)
                                              (unpack $ particleVelocity particle)
                                              (unpack $ particlePos particle)

    calcVelocity :: (PrimMonad m, Vector v a, Num a) => Params a -> v a -> v a -> v a -> v a -> v a -> v a -> m (v a)
    calcVelocity params@(Params omega phiP phiL) rP rL bestPPos bestLPos v pos = do
        -- locx <- VG.zipWithM (\blp pos -> return $ blp - pos) bestLPos pos
        -- loc  <- VG.zipWithM (\r l -> return $ phiL * r * l) rL locx
        -- parx <- VG.zipWithM (\bpp pos -> return $ bpp - pos) bestPPos pos
        -- par  <- VG.zipWithM (\r p -> return $ phiP * r * p) rP parx
        -- lpar <- VG.zipWithM (\l p -> return $ l + p) loc par
        -- let vels = VG.map (\vi -> omega * vi) v
        -- VG.zipWithM (\v s -> return $ v + s) vels lpar
        VG.izipWithM (calcVel params rP rL bestPPos bestLPos) v pos

    calcVel :: (PrimMonad m, Num a, Vector v a) => Params a -> v a -> v a -> v a -> v a -> Int -> a -> a -> m a
    calcVel (Params omega phiP phiL) rP rL bestPPos bestLPos i v pos = do
        let rPI = rP VG.! i
            rLI = rL VG.! i
            bestPPosI = bestPPos VG.! i
            bestLPosI = bestLPos VG.! i
        -- return $ omega * v
        return $ omega * v + rPI * phiP * (bestPPosI - pos) + rLI * phiL * (bestLPosI - pos)

-- zipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
--              Vector v f, Vector v g)
--          => (a -> b -> c -> d -> e -> f -> g)
--          -> v a -> v b -> v c -> v d -> v e -> v f -> v g
-- {-# INLINE zipWith6 #-}
-- zipWith6 f = \as bs cs ds es fs ->
--     unstream (Bundle.zipWith6 f (stream as)
--                                 (stream bs)
--                                 (stream cs)
--                                 (stream ds)
--                                 (stream es)
--                                 (stream fs))

-- zipWithM :: (Monad m, Vector v a, Vector v b, Vector v c)
--          => (a -> b -> m c) -> v a -> v b -> m (v c)
-- -- FIXME: specialise for ST and IO?
-- {-# INLINE zipWithM #-}
-- zipWithM f = \as bs -> unstreamM $ Bundle.zipWithM f (stream as) (stream bs)

    calcVelocityI :: (PrimMonad m, Num a) => Params a -> a -> a -> a -> a -> a -> a -> m a
    calcVelocityI (Params omega phiP phiL) rP rL bestPPos bestLPos v pos = return $ omega * v + rP * phiP * (bestPPos - pos) + rL * phiL * (bestLPos - pos)
    -- v ← ωv + φp rp (p-x) + φg rg (g-x)

genRandomVector :: PrimMonadPrim m => Dim -> Rand m RandVector
genRandomVector (Dim d) = fmap RandVector $ VU.replicateM d $ RM.uniformR rRange

updateParticlePosVel :: PrimMonadPrim m => Dim -> Evaluator -> Particle -> Rand m Particle
updateParticlePosVel dim evaluator particle = do
    rPV <- genRandomVector dim
    rLV <- genRandomVector dim
    liftPrim $ updateParticleVelPosEval evaluator rPV rLV particle
    -- return $ updateParticleVelPosEval evaluator rPV rLV particle

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

-- TODO
-- thaw

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
