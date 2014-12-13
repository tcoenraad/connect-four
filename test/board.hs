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

    it "should not be able to drop a coin in a column that does not exist" $ do
      dropCoin [[],[],[],[],[],[],[]]
        Yellow 7 `shouldBe` Nothing
      dropCoin [[],[],[],[],[],[],[]]
        Yellow (-1) `shouldBe` Nothing
