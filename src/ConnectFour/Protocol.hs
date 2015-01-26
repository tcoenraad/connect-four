module ConnectFour.Protocol where

  import qualified ConnectFour.Board as Board

  handshake :: [String] -> Bool
  handshake s = head s == "hello" && length s == 3

  play :: [String] -> Bool
  play s = head s == "play" && length s == 1

  move :: [String] -> Bool
  move s = head s == "move" && length s == 2 && r /= [] && row >= 0 && row < Board.columns where
    r = reads (last s) :: [(Int, String)]
    row = fst $ head r

  true :: String
  true = "true"

  false :: String
  false = "false"

  ack :: String
  ack = "hello"

  gameStarted :: String
  gameStarted = "makeGame"

  moveDone :: String
  moveDone = "makeMove"

  gameOver :: String
  gameOver = "gameOver"

  sendPlayers :: String
  sendPlayers = "sendPlayers"

  boolTrue :: Char
  boolTrue = '1'

  boolFalse :: Char
  boolFalse = '0'

  errorNameInUse :: String
  errorNameInUse = "error invalidName"

  errorUnknownCommand :: String
  errorUnknownCommand = "error invalidCommand"

  errorInvalidMove :: String
  errorInvalidMove = "error invalidMove"
