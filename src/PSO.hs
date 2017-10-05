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
import           Functions (CostFunction, Evaluator(..), BoundsList(..), Bounds(..))
import qualified Functions as F

-- TODO:
-- what velocity, velocity changes, global propagation

newtype Position = Position (VU.Vector Double)
    deriving (Show, Generic, Newtype)

newtype Velocity = Velocity (VU.Vector Double)
    deriving (Show, Generic, Newtype)

newtype Value = Value Double
    deriving (Show, Eq, Ord, Generic, Newtype)

data EvaluatedPositon = EvaluatedPositon
    { position :: Position
    , value    :: Value
    } deriving (Show)

data Particle = Particle
    { particleEvalPosition :: EvaluatedPositon
    , particleVelocity     :: Velocity
    , bestEvaluation       :: EvaluatedPositon
    , bestGlobalEvaluation :: EvaluatedPositon
    } deriving (Show)

newtype Swarm = Swarm (V.Vector Particle)
    deriving (Show, Generic, Newtype)

type RandMonad m = (PrimMonad m, MonadPrim m)

genVectorFor :: RandMonad m => (VU.Vector Double -> a) -> BoundsList -> Rand m a
genVectorFor cons (BoundsList boundsList) = do
    vecB <- forM boundsList createVec
    return . cons $ V.convert vecB
  where
    createVec (Bounds l u) = RM.uniformR (l, u)

genPosition :: RandMonad m => BoundsList -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: RandMonad m => BoundsList -> Rand m Velocity
genVelocity = genVectorFor Velocity

evalCost :: Evaluator -> Position -> Value
evalCost (Evaluator eval) (Position pos) = Value $ eval pos

genParticle :: RandMonad m => BoundsList -> EvaluatedPositon -> EvaluatedPositon -> Rand m Particle
genParticle boundsList bestGlobalEval evalPos = do
    vel <- genVelocity boundsList -- TODO: scale
    return $ Particle evalPos vel evalPos bestGlobalEval

genEvaluatedPosition :: RandMonad m => CostFunction -> Rand m EvaluatedPositon
genEvaluatedPosition costFunction = do
    pos <- genPosition $ F.boundsList costFunction
    return . EvaluatedPositon pos $ evalCost (F.evaluator costFunction) pos

genSwarm :: RandMonad m => Int -> CostFunction -> Rand m Swarm
genSwarm swarmSize costFunction = do
    evaluatedPositions <- V.replicateM swarmSize $ genEvaluatedPosition costFunction
    let bestEvalPos = V.minimumBy (comparing value) evaluatedPositions
    fmap Swarm . forM evaluatedPositions $ genParticle (F.boundsList costFunction) bestEvalPos

optimize :: RandMonad m => Swarm -> Integer -> Rand m Particle
optimize swarm iterations = do
    return $ V.head $ unpack swarm

test :: Config -> IO ()
test cfg = do
    let genSeed = RM.toSeed . VU.singleton $ Config.seed cfg
        dim = Config.dimension cfg
        costFunction = F.rastrigin dim
    result <- RM.runWithSeed genSeed $ do
        initialSwarm <- genSwarm (Config.swarmSize cfg) costFunction
        liftIO $ putStrLn $ "Initial swarm:\n" <> ppShow initialSwarm
        optimize initialSwarm $ fromMaybe 100 $ Config.iterations cfg
    -- putStrLn $ "Result:\n" <> (renderStyle mystyle $ ppDoc result)
    putStrLn $ "Result:\n" <> ppShow result
    putStrLn $ "Cost function bounds:\n" <> ppShow (F.boundsList costFunction)
    -- putStrLn $  renderStyle mystyle $ ppDoc [1..25]
