module ConnectFour.Protocol where

  handshake :: [String] -> Bool
  handshake s = head s == "hello" && length s == 3

  true :: Char
  true = '1'

  errorNameInUse :: String
  errorNameInUse = "error invalidName"
