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

  chat :: [String] -> Bool
  chat s = head s == "chat" && length s >= 2

  challenge :: [String] -> Bool
  challenge s = head s == "challenge" && length s == 2

  acceptChallenge :: [String] -> Bool
  acceptChallenge s = head s == "acceptChallenge" && length s == 1

  rejectChallenge :: [String] -> Bool
  rejectChallenge s = head s == "rejectChallenge" && length s == 1

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

  sendChat :: String
  sendChat = "sendChat"

  challenged :: String
  challenged = "challenged"

  challengeCancelled :: String
  challengeCancelled = "challengeCancelled"

  reject :: String
  reject = "reject"

  supported :: String
  supported = "110"

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

  errorInvalidClient :: String
  errorInvalidClient = "error invalidClient"
