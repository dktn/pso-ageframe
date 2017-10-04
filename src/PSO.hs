{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}

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

-- import           Data.Default (Default(..))

import           Pretty (ppShow)
import           Config (Config())
import qualified Config
import           Functions (CostFunction, Evaluator(..), Bounds(..), Dim(..))
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
    } deriving (Show, Eq, Ord)

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

newtype Swarm = Swarm
    { particles :: V.Vector Particle
    } deriving (Show)

type RandMonad m = (PrimMonad m, MonadPrim m)

genVectorFor :: RandMonad m => (VU.Vector Double -> a) -> Dim -> Bounds -> Rand m a
genVectorFor cons (Dim dim) (Bounds l u) = fmap cons $ VU.replicateM dim $ RM.uniformR (l, u)

genPosition :: RandMonad m => Dim -> Bounds -> Rand m Position
genPosition = genVectorFor Position

genVelocity :: RandMonad m => Dim -> Bounds -> Rand m Velocity
genVelocity = genVectorFor Velocity

evalCost :: Evaluator -> Position -> Value
evalCost (Evaluator eval) (Position pos) = Value $ eval pos

genParticle :: RandMonad m => CostFunction -> EvaluatedPositon -> EvaluatedPositon -> EvaluatedPositon -> Rand m Particle
genParticle costFunction bestEval bestGlobalEval evalPos = do
    let dim = F.dim costFunction
        Bounds l u = F.bounds costFunction
    vel <- genVelocity dim $ Bounds (l / 100.0) (u / 100.0)
    return $ Particle evalPos vel bestEval bestGlobalEval

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
    fmap Swarm $ forM evaluatedPositions $ genParticle costFunction bestEvalPos bestEvalPos

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
