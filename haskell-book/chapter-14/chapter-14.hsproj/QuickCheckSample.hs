module QuickCheckSample where
  
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)