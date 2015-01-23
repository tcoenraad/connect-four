module ConnectFour.GameSpec where

  import Test.Hspec
  import Data.Maybe

  import ConnectFour.Game
  import ConnectFour.Board hiding (dropCoin, winningColumn)

  emptyBoard :: Board
  emptyBoard = [[],[],[],[],[],[],[]]

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do

    describe "Game" $ do

      context "when alternating turns" $ do

        it "should alter turns for two players" $ do
          let g = Game { board = emptyBoard, amountPlayers = 2, currentPlayer = 0 }
          let g' = fromJust $ dropCoin g 0
          let g'' = fromJust $ dropCoin g' 1
          let Game { board=board' } = fromJust $ dropCoin g'' 2
          board' `shouldBe` [[Red],[Green],[Red],[],[],[],[]]

        it "should alter turns for three players" $ do
          let g = Game { board = emptyBoard, amountPlayers = 3, currentPlayer = 0 }
          let g' = fromJust $ dropCoin g 0
          let g'' = fromJust $ dropCoin g' 1
          let g''' = fromJust $ dropCoin g'' 2
          let Game { board=board' } = fromJust $ dropCoin g''' 3
          board' `shouldBe` [[Red],[Green],[Blue],[Red],[],[],[]]  

        it "should alter turns for four players" $ do
          let g = Game { board = emptyBoard, amountPlayers = 4, currentPlayer = 0 }
          let g' = fromJust $ dropCoin g 0
          let g'' = fromJust $ dropCoin g' 1
          let g''' = fromJust $ dropCoin g'' 2
          let g'''' = fromJust $ dropCoin g''' 3
          let Game { board=board' } = fromJust $ dropCoin g'''' 4
          board' `shouldBe` [[Red],[Green],[Blue],[Yellow],[Red],[],[]]

      context "when doing a winning move" $ do

        it "should detect a winning column" $ do
          let g = Game { board = emptyBoard, amountPlayers = 2, currentPlayer = 0 }
          let g' = fromJust $ dropCoin g 0
          let g'' = fromJust $ dropCoin g' 1
          let g''' = fromJust $ dropCoin g'' 0
          let g'''' = fromJust $ dropCoin g''' 1
          let g''''' = fromJust $ dropCoin g'''' 0
          let g'''''' = fromJust $ dropCoin g''''' 1
          let g''''''' = fromJust $ dropCoin g'''''' 0
          winningColumn g'''''' 0 `shouldBe` False
          winningColumn g''''''' 0 `shouldBe` True
