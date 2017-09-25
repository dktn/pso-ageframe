module Platform
    ( runPlatform
    ) where

import           Config (Config)
import           PSO (test)

runPlatform :: Config -> IO ()
runPlatform cfg = do
    putStrLn $ show cfg
    test cfg
