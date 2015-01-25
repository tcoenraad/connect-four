{-# LANGUAGE DeriveGeneric #-}

module ConnectFour.Game where

  import Data.Maybe
  import GHC.Generics

  import qualified ConnectFour.Board as Board
  import qualified Data.Aeson as Aeson

  data Game = Game {
    board :: Board.Board,
    currentPlayer :: Int,
    amountPlayers :: Int
  } deriving (Eq, Generic, Show)

  instance Aeson.ToJSON Game

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

  winningColumn :: Game -> Int -> Bool
  winningColumn Game{board=b} x = Board.winningColumn b x

  fullBoard :: Game -> Bool
  fullBoard Game{board=b} = Board.full b

  initalizeGame :: Game
  initalizeGame = Game {
                    board = [[],[],[],[],[],[],[]],
                    currentPlayer = 0,
                    amountPlayers = 2
                  }
