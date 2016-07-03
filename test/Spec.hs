import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "stateful" $ do
    it "carries state" $ do
      (streamTest 6 $ stateful 2 (+3)) `shouldBe` [2,5,8,11,14,17,20]
  describe "counters" $ do
    it "countdown" $ do
      streamTest 5 $ countdown "foo" 4
      `shouldBe` [("foo",Just 4),("foo",Just 3),("foo",Just 2),("foo",Just 1),("foo",Just 0),("foo",Nothing)]
    it "timers" $ do
      streamTest 8 $ timers [("a", 3, 0), ("b", 5, 1), ("c", 3, 1), ("d", 4, 3)]
      `shouldBe`
      [[("a",3)],[("b",5),("c",3),("a",2)],
       [("b",4),("c",2),("a",1)],
       [("d",4),("b",3),("c",1),
        ("a",0)],[("d",3),("b",2),("c",0)],
        [("d",2),("b",1)],
        [("d",1),("b",0)],
        [("d",0)],
        []]
