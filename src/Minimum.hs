{-# OPTIONS_GHC -Wno-unused-matches #-}

module Minimum where

import           Protolude
import           System.Random.MWC.Monad (Rand(..))
import qualified System.Random.MWC.Monad as RM
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Primitive.Class (MonadPrim) -- liftPrim
import qualified Data.Vector.Unboxed as VU

import           Pretty (ppShow)
import           Config (Config())
import qualified Config

type PrimMonadPrim m = (PrimMonad m, MonadPrim m)

type Comp m a = ReaderT Config (StateT Int (Rand m)) a

runComp :: PrimMonadPrim m => RM.Seed -> Config -> Int -> Comp m a -> m a
runComp seed cfg st comp = RM.runWithSeed seed (evalStateT (runReaderT comp cfg) st)

liftRand :: (MonadPrim m) => Rand m a -> Comp m a
liftRand = lift . lift

getRandomVal :: (MonadPrim m) => Rand m Double
getRandomVal = RM.uniformR (0.0 :: Double, 100.0)

computation :: (PrimMonadPrim m, MonadIO m) => Comp m Double
computation = do
    cfg <- ask
    abc <- get
    liftRand getRandomVal


test :: Config -> IO ()
test cfg = do
    seedNum <- Config.genTimeSeed
    let genSeed = RM.toSeed $ VU.singleton seedNum
    res <- runComp genSeed cfg 8 computation
    putText $ "result:\n" <> ppShow res
