module Board where

  data Coin = Red | Green | Blue | Yellow | Empty deriving (Eq, Show)
  type Column = [Coin]
  type Board = [Column]

  rows :: Int
  rows = 6

  columns :: Int
  columns = 7

  dropCoin :: Board -> Coin -> Int -> Maybe Board
  dropCoin (c:cs) coin 0 | length c >= rows = Nothing
                         | otherwise = Just $ (c ++ [coin]) : cs
  dropCoin (c:cs) coin i | i < 0 || i >= columns = Nothing
                         | otherwise = (dropCoin cs coin (i - 1)) >>= \x -> Just (c : x)
