import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Criterion.Main

import Data.ByteString.Arbitrary

main :: IO ()
main = defaultMain
  [ bgroup "ArbByteString"
    [ bench "arbitrary"    $ action (arbitrary :: Gen ArbByteString)
    , bench "arbitrary1M"  $ action (arbitrary :: Gen ArbByteString1M)
    , bench "arbitrary10M" $ action (arbitrary :: Gen ArbByteString10M)
    , bgroup "fastRandBs"
      [ bench "10B" $ action (fastRandBs 10)
      , bench "1KB" $ action (fastRandBs 1024)
      , bench "1MB" $ action (fastRandBs (1024*1024))
      ]
    , bgroup "slowRandBs"
      [ bench "10B" $ action (slowRandBs 10)
      , bench "1KB" $ action (slowRandBs 1024)
      , bench "10KB" $ action (slowRandBs 10240)
      ]
    ]
  ]

  where
  action :: Gen a -> IO ()
  action act = do
    samples <- sample' act
    examine samples
    where
    examine :: [a] -> IO ()
    examine [] = return ()
    examine (hd:tl) = hd `seq` examine tl
