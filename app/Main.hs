module Main where

import           Control.Concurrent       (threadDelay)
import           Data.Maybe               (listToMaybe)
import           System.Environment       (getArgs)
import           System.Exit              (die)
import           System.Remote.Counter    (Counter, inc)
import           System.Remote.Monitoring (forkServer, getCounter)

import           Config
import           Platform

getConfig :: IO Config
getConfig = do
    args <- getArgs
    file <- case listToMaybe args of
        Nothing  -> die "No config file provided."
        Just arg -> return arg
    cfgMay <- Config.loadConfig file
    case cfgMay of
        Left  err -> die err
        Right cfg -> return cfg

runEkg :: IO Counter
runEkg = do
    ekg <- forkServer "localhost" 8000
    getCounter "iterations" ekg

test :: Counter -> IO ()
test counter = loop where
    loop = do
        inc counter
        threadDelay 10000
        loop

main :: IO ()
main = do
    counter <- runEkg
    cfg     <- getConfig
    runPlatform cfg
    -- test counter
    -- ben
