module Platform
    ( runPlatform
    ) where

import           Config             (Config)

runPlatform :: Config -> IO ()
runPlatform cfg = do
    putStrLn $ show cfg
