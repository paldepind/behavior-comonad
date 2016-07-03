import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "stateful" $ do
    it "carries state" $ do
      (streamTest 6 $ stateful 2 (+3)) `shouldBe` [2,5,8,11,14,17,20]
