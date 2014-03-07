import qualified Data.ByteString as BS
import Data.ByteString ( ByteString )
import Test.QuickCheck.Monadic
import Test.QuickCheck

import Data.ByteString.Arbitrary ( fastRandBs, slowRandBs )

main :: IO ()
main = do
  quickCheck $ lengths fastRandBs
  quickCheck $ lengths (\i -> slowRandBs $ min i 10240)
  where
  lengths :: (Int -> Gen ByteString) -> Int -> Property
  lengths genFn len =
    let gen = genFn len
    in monadicIO $ do
        samples <- run $ sample' gen
        return $ all (==len) $ map BS.length samples

