module ConnectFour.Board where

  data Coin = Red | Green | Blue | Yellow | Empty deriving (Eq, Show)
  type Column = [Coin]
  type Board = [Column]
  type Pos = (Int, Int)

  rows :: Int
  rows = 6

  columns :: Int
  columns = 7

  strike :: Int
  strike = 4

  index :: Board -> Pos -> Maybe Coin
  index b (x, y) | x < 0 || x >= columns = Nothing
                 | y < 0 || y >= rows = Nothing
                 | length column <= y = Just Empty
                 | otherwise = Just $ column !! y
                 where
                   column = b !! x

  dropCoin :: Board -> Coin -> Int -> Maybe Board
  dropCoin b coin i | length column >= rows = Nothing
                    | otherwise = Just $ (take i b) ++ [(column ++ [coin])] ++ (drop (i + 1) b)
                    where
                      column = b !! i

  full :: Board -> Bool
  full [] = True
  full (c:cs) = length c == rows && full cs

  winningColumn :: Board -> Int -> Bool
  winningColumn b x = winningPos b (x, y)
                    where
                      y = length (b !! x) - 1

  winningPos :: Board -> Pos -> Bool
  winningPos b (x, y) = south >= strike
                      || west + east - 1 >= strike
                      || southwest + northeast - 1 >= strike
                      || northwest + southeast - 1 >= strike
    where
      west = consecutiveCoinsLength b (x, y) (-1, 0) 0
      east = consecutiveCoinsLength b (x, y) (1, 0) 0
      south = consecutiveCoinsLength b (x, y) (0, -1) 0
      southwest = consecutiveCoinsLength b (x, y) (-1, -1) 0
      northeast = consecutiveCoinsLength b (x, y) (1, 1) 0
      northwest = consecutiveCoinsLength b (x, y) (-1, 1) 0
      southeast = consecutiveCoinsLength b (x, y) (1, -1) 0

  consecutiveCoinsLength :: Board -> Pos -> Pos -> Int -> Int
  consecutiveCoinsLength b (x, y) (diff_x, diff_y) offset | offset == strike = 0
                                                          | coin == coin' = 1 + consecutiveCoinsLength b (x, y) (diff_x, diff_y) (offset + 1)
                                                          | otherwise = 0
    where
      coin = index b (x, y)
      coin' = index b (x + diff_x * offset, y + diff_y * offset)
