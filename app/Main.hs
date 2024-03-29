module Main where

import           Protolude
import           Control.Concurrent       (threadDelay)
import           System.Environment       (getArgs)
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
    cfgMay <- Config.loadConfig $ toS file
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
    inc counter
    putText "Exited."
    -- test counter
    -- ben
