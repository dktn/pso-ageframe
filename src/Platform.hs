module Platform
    ( runPlatform
    ) where

import           Config (Config)
import           PSO

runPlatform :: Config -> IO ()
runPlatform cfg = do
    putStrLn $ show cfg
    -- testExplicitGen cfg
    test cfg
