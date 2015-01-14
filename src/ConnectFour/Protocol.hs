module ConnectFour.Protocol where

  handshake :: String -> Bool
  handshake = (==) "hello"
