module ConnectFour.Protocol where

  import qualified ConnectFour.Board as Board

  handshake :: [String] -> Bool
  handshake s = head s == "hello" && length s == 3

  play :: [String] -> Bool
  play s = head s == "play" && length s == 1

  gameStarted :: String
  gameStarted = "makeGame"

  moveDone :: String
  moveDone = "move"

  move :: [String] -> Bool
  move s = head s == "move" && length s == 2 && read (last s) < Board.rows

  true :: Char
  true = '1'

  errorNameInUse :: String
  errorNameInUse = "error invalidName"

  errorUnknownCommand :: String
  errorUnknownCommand = "error invalidCommand"

  errorInvalidMove :: String
  errorInvalidMove = "error invalidMove"
