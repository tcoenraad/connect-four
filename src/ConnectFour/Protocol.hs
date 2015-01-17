module ConnectFour.Protocol where

  import Debug.Trace

  handshake :: [String] -> Bool
  handshake s = head s == "hello" && length s == 3

  play :: [String] -> Bool
  play s = head s == "play" && length s == 1

  gameStarted :: String
  gameStarted = "makeGame"

  true :: Char
  true = '1'

  errorNameInUse :: String
  errorNameInUse = "error invalidName"

  errorUnknownCommand :: String
  errorUnknownCommand = "error invalidCommand"
