module Board where

  data Coin = Red | Green | Blue | Yellow | Empty deriving (Eq, Show)
  type Column = [Coin]
  type Board = [Column]

  rows :: Int
  rows = 6

  columns :: Int
  columns = 7

  strike :: Int
  strike = 4

  index :: Board -> (Int, Int) -> Maybe Coin
  index b (x, y) | x < 0 || x >= columns = Nothing
                 | y < 0 || y >= rows = Nothing
                 | length column <= y = Just Empty
                 | otherwise = Just $ column !! y
                 where
                   column = (b !! x)

  dropCoin :: Board -> Coin -> Int -> Maybe Board
  dropCoin b coin i | length column >= rows = Nothing
                    | otherwise = Just $ (take i b) ++ [(column ++ [coin])] ++ (drop (i + 1) b)
                    where
                      column = (b !! i)

  winningMove :: Board -> (Int, Int) -> Bool
  winningMove b (x, y) = left + right >= strike
                        || down >= strike
    where
      left = consecutive_coins_length b (x, y) (-1, 0) 0
      right = consecutive_coins_length b (x, y) (1, 0) 0
      down = consecutive_coins_length b (x, y) (0, -1) 0

  consecutive_coins_length :: Board -> (Int, Int) -> (Int, Int) -> Int -> Int
  consecutive_coins_length b (x, y) (diff_x, diff_y) shift | shift == strike = 0
                                                           | coin == coin' = 1 + consecutive_coins_length b (x, y) (diff_x, diff_y) (shift + 1)
                                                           | otherwise = 0
    where
      coin = index b (x, y)
      coin' = index b (x + diff_x * shift, y + diff_y * shift)
