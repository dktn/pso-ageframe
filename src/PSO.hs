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

-- import           Data.Default (Default(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Evaluator(..), BoundsList(..), Bounds(..), Dim(..))
import qualified Functions as F

-- TODO:
-- bounds per dimension
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

genVectorFor :: RandMonad m => (VU.Vector Double -> a) -> Dim -> Bounds -> Rand m a
genVectorFor cons (Dim d) (Bounds l u) = fmap cons $ VU.replicateM d $ RM.uniformR (l, u)

genPosition :: RandMonad m => Dim -> Bounds -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: RandMonad m => Dim -> Bounds -> Rand m Velocity
genVelocity = genVectorFor Velocity

evalCost :: Evaluator -> Position -> Value
evalCost (Evaluator eval) (Position pos) = Value $ eval pos

genParticle :: RandMonad m => CostFunction -> EvaluatedPositon -> EvaluatedPositon -> Rand m Particle
genParticle costFunction bestGlobalEval evalPos = do
    let dim = F.dim costFunction
        Bounds l u = F.bounds costFunction
    vel <- genVelocity dim $ Bounds (l / 100.0) (u / 100.0)
    return $ Particle evalPos vel evalPos bestGlobalEval

genEvaluatedPosition :: RandMonad m => CostFunction -> Rand m EvaluatedPositon
genEvaluatedPosition costFunction = do
    let dim = F.dim costFunction
        evaluator = F.evaluator costFunction
        bounds = F.bounds costFunction
    pos <- genPosition dim bounds
    let val = evalCost evaluator pos
    return $ EvaluatedPositon pos val

genSwarm :: RandMonad m => Int -> CostFunction -> Rand m Swarm
genSwarm swarmSize costFunction = do
    evaluatedPositions <- V.replicateM swarmSize $ genEvaluatedPosition costFunction
    let bestEvalPos = V.minimumBy (comparing value) evaluatedPositions
    fmap Swarm $ forM evaluatedPositions $ genParticle costFunction bestEvalPos

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
    putStrLn $ "Cost function:\n" <> ppShow (F.bounds costFunction)
    -- putStrLn $  renderStyle mystyle $ ppDoc [1..25]
