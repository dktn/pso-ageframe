module Main where

import           Protolude
import           Criterion.Main


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain [
       bgroup "fib" [ bench "10" $ whnf fib 10
                    , bench "35" $ whnf fib 35
                    , bench "37" $ whnf fib 37
                    ]
                   ]



-- fact :: Integ -> Integer
-- fact 1 = 1
-- fact n = n * fact (pred n)

-- ben :: IO ()
-- ben = defaultMainWith
--   defaultConfig { cfgSamples = ljust 1000 }
--   (return ())
--   [ bench "fact 30" $ nf fact 30
--   , bench "fact 40" $ nf fact 40
--   ]
