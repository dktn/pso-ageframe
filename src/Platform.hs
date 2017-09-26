module Platform
    ( runPlatform
    ) where

import           Config (Config)
import           PSO

runPlatform :: Config -> IO ()
runPlatform cfg = do
    putStrLn $ show cfg
    test1 cfg
    test2 cfg
