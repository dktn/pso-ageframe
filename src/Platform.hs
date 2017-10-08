module Platform
    ( runPlatform
    ) where

import           Protolude

import           Config (Config)
import           Pretty
import           PSO
-- import qualified Minimum

runPlatform :: Config -> IO ()
runPlatform cfg = do
    putStrLn $ ppShow cfg
    -- testExplicitGen cfg
    test cfg
    -- Minimum.test cfg
