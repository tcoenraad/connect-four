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

    it "should detect a horizontal strike from the start" $ do
      winningMove [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] (0, 0) `shouldBe` True

    it "should detect a horizontal strike halfway" $ do
      winningMove [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] (2, 0) `shouldBe` True

    it "should detect a horizontal strike on the end" $ do
      winningMove [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] (3, 0) `shouldBe` True

    it "should detect a not connected horizontal strike" $ do
      winningMove [[Yellow],[Yellow],[],[Yellow],[Yellow],[],[]] (1, 0) `shouldBe` False

    it "should detect a vertical strike on the end" $ do
      winningMove [[Yellow,Yellow,Yellow,Yellow],[],[],[],[],[],[]] (0, 3) `shouldBe` True

    it "should detect a not connected vertical strike" $ do
      winningMove [[Yellow,Yellow,Empty,Yellow,Yellow],[],[],[],[],[],[]] (0, 1) `shouldBe` False

    it "should detect an increasing diagonal strike from the start" $ do
      winningMove [[Yellow],[Empty,Yellow],[Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[],[]] (0, 0) `shouldBe` True

    it "should detect an increasing diagonal strike halfway" $ do
      winningMove [[Yellow],[Empty,Yellow],[Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[],[]] (2, 2) `shouldBe` True

    it "should detect an increasing diagonal strike on the end" $ do
      winningMove [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] (3, 3) `shouldBe` True

    it "should detect a not connected increasing diagonal strike" $ do
      winningMove [[Yellow],[Empty,Yellow],[],[Empty,Empty,Empty,Yellow],[Empty,Empty,Empty,Empty,Yellow],[],[]] (1, 1) `shouldBe` False

    it "should detect an decreasing diagonal strike from the start" $ do
      winningMove [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] (0, 3) `shouldBe` True

    it "should detect an decreasing diagonal strike halfway" $ do
      winningMove [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] (2, 1) `shouldBe` True

    it "should detect an decreasing diagonal strike on the end" $ do
      winningMove [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] (3, 0) `shouldBe` True

    it "should detect a not connected decreasing diagonal strike" $ do
      winningMove [[Empty,Empty,Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[Empty,Yellow],[Yellow],[],[]] (1, 3) `shouldBe` False
