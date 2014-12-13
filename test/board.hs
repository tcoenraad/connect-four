import Test.Hspec
import Board

main :: IO ()
main = hspec $ do
  describe "Board" $ do
    it "should be able to drop a coin in an empty column" $ do
      dropCoin [[],[],[],[],[],[],[]]
        Yellow 0 `shouldBe` Just [[Yellow],[],[],[],[],[],[]]
      dropCoin [[],[],[],[],[],[],[]]
        Yellow 6 `shouldBe` Just [[],[],[],[],[],[],[Yellow]]

    it "should be able to drop a coin in a column with coins" $ do
      dropCoin [[Yellow],[],[],[],[],[],[]]
        Yellow 0 `shouldBe` Just [[Yellow, Yellow],[],[],[],[],[],[]]

    it "should not be able to drop a coin in a column that is full" $ do
      dropCoin [[Yellow, Yellow, Yellow, Yellow, Yellow, Yellow],[],[],[],[],[],[]]
        Yellow 0 `shouldBe` Nothing

    it "should give the right coin on the lower left position" $ do
      index [[Yellow],[],[],[],[],[],[]]
        (0,0) `shouldBe` Just Yellow

    it "should give the right coin on the upper right position" $ do
      index [[],[],[],[],[],[],[Empty, Empty, Empty, Empty, Empty, Yellow]]
        (6,5) `shouldBe` Just Yellow

    it "should give the Empty coin on a valid, yet empty position" $ do
      index [[],[],[],[],[],[],[]] (0, 0) `shouldBe` Just Empty

    it "should not give any coin on an invalid position" $ do
      index [[],[],[],[],[],[],[]] (-1, 0) `shouldBe` Nothing
      index [[],[],[],[],[],[],[]] (0, -1) `shouldBe` Nothing
      index [[],[],[],[],[],[],[]] (7, 5) `shouldBe` Nothing
      index [[],[],[],[],[],[],[]] (6, 6) `shouldBe` Nothing
