module ConnectFour.Protocol where

  handshake :: [String] -> Bool
  handshake s = head s == "hello" && length s == 3
