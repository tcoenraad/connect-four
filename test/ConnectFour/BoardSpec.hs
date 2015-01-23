module ConnectFour.BoardSpec where

  import Test.Hspec
  import ConnectFour.Board

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do

    describe "Board" $ do

      context "when dropping coins on board" $ do

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

      context "when finding fields" $ do

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

      context "when winning moves on board are made horizontally" $ do

        it "should detect a horizontal strike from the start" $ do
          winningColumn [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] 0 `shouldBe` True

        it "should detect a horizontal strike halfway" $ do
          winningColumn [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] 2 `shouldBe` True

        it "should detect a horizontal strike on the end" $ do
          winningColumn [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] 3 `shouldBe` True

        it "should detect a not long enough horizontal strike" $ do
          winningColumn [[Yellow],[Yellow],[Yellow],[],[],[],[]] 1 `shouldBe` False

        it "should detect a not connected horizontal strike" $ do
          winningColumn [[Yellow],[Yellow],[],[Yellow],[Yellow],[],[]] 1 `shouldBe` False

      context "when winning moves on board are made vertically" $ do
        it "should detect a vertical strike on the end" $ do
          winningColumn [[Yellow,Yellow,Yellow,Yellow],[],[],[],[],[],[]] 0 `shouldBe` True

        it "should detect a not long enough horizontal strike" $ do
          winningColumn [[Yellow,Yellow,Yellow],[],[],[],[],[],[]] 0 `shouldBe` False

        it "should detect a not connected vertical strike" $ do
          winningColumn [[Yellow,Yellow,Empty,Yellow,Yellow],[],[],[],[],[],[]] 0 `shouldBe` False

      context "when winning moves on board are made diagonally" $ do

        it "should detect an increasing diagonal strike from the start" $ do
          winningColumn [[Yellow],[Empty,Yellow],[Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[],[]] 0 `shouldBe` True

        it "should detect an increasing diagonal strike halfway" $ do
          winningColumn [[Yellow],[Empty,Yellow],[Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[],[]] 2 `shouldBe` True

        it "should detect an increasing diagonal strike on the end" $ do
          winningColumn [[Yellow],[Yellow],[Yellow],[Yellow],[],[],[]] 3 `shouldBe` True

        it "should detect a not long enough increasing diagonal strike" $ do
          winningColumn [[Yellow],[Empty,Yellow],[Empty,Empty,Yellow],[],[],[],[]] 1 `shouldBe` False

        it "should detect a not connected increasing diagonal strike" $ do
          winningColumn [[Yellow],[Empty,Yellow],[],[Empty,Empty,Empty,Yellow],[Empty,Empty,Empty,Empty,Yellow],[],[]] 1 `shouldBe` False

        it "should detect an decreasing diagonal strike from the start" $ do
          winningColumn [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] 0 `shouldBe` True

        it "should detect an decreasing diagonal strike halfway" $ do
          winningColumn [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] 2 `shouldBe` True

        it "should detect an decreasing diagonal strike on the end" $ do
          winningColumn [[Empty,Empty,Empty,Yellow],[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[]] 3 `shouldBe` True

        it "should detect a not long enough decreasing diagonal strike" $ do
          winningColumn [[Empty,Empty,Yellow],[Empty,Yellow],[Yellow],[],[],[],[]] 1 `shouldBe` False

        it "should detect a not connected decreasing diagonal strike" $ do
          winningColumn [[Empty,Empty,Empty,Empty,Yellow],[Empty,Empty,Empty,Yellow],[],[Empty,Yellow],[Yellow],[],[]] 1 `shouldBe` False

      context "when board is about to be full" $ do

        it "should detect an almost full board to be not full" $ do
          let fullColumn = [Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]
          full [fullColumn,fullColumn,fullColumn,fullColumn,fullColumn,take 5 fullColumn] `shouldBe` False

        it "should detect an almost full board to be not full" $ do
          let fullColumn = [Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]
          full [fullColumn,fullColumn,fullColumn,fullColumn,fullColumn,fullColumn] `shouldBe` True
