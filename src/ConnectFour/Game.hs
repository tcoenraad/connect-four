module ConnectFour.Game where

  import Data.Maybe
  import qualified ConnectFour.Board as Board

  data Game = Game {
    board :: Board.Board,
    currentPlayer :: Int,
    amountPlayers :: Int
  }

  coin :: Int -> Board.Coin
  coin 0 = Board.Red
  coin 1 = Board.Green
  coin 2 = Board.Blue
  coin 3 = Board.Yellow

  dropCoin :: Game -> Int -> Maybe Game
  dropCoin Game{board = b, currentPlayer = cp, amountPlayers = ap} i | isJust b' = Just Game { board = fromJust b', currentPlayer = cp', amountPlayers = ap}
                                                                     | otherwise = Nothing where
    b' = Board.dropCoin b (coin cp) i
    cp' = (cp + 1) `mod` ap

  initalizeGame :: Game
  initalizeGame = Game {
                    board = [[],[],[],[],[],[],[]],
                    currentPlayer = 0,
                    amountPlayers = 2
                  }
