module ConnectFour.ServerSpec where

  import Test.Hspec

  import ConnectFour.Server

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do

    describe "Server" $ do

      context "when adding players to queue" $ do

        it "should handle two players" $ do
          (queue "Test1" [] 2) `shouldBe` Right ["Test1"]
          (queue "Test2" ["Test1"] 2) `shouldBe` Left ["Test1", "Test2"]

        it "should handle three players" $ do
          (queue "Test2" ["Test1"] 3) `shouldBe` Right ["Test1", "Test2"]
          (queue "Test3" ["Test1", "Test2"] 3) `shouldBe` Left ["Test1", "Test2", "Test3"]

        it "should handle three players" $ do
          (queue "Test3" ["Test1", "Test2"] 4) `shouldBe` Right ["Test1", "Test2", "Test3"]
          (queue "Test4" ["Test1", "Test2", "Test3"] 4) `shouldBe` Left ["Test1", "Test2", "Test3", "Test4"]
