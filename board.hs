module Board where

  data Coin = Red | Green | Blue | Yellow | Empty deriving (Eq, Show)
  type Column = [Coin]
  type Board = [Column]

  rows :: Int
  rows = 6

  columns :: Int
  columns = 7

  index :: Board -> (Int, Int) -> Maybe Coin
  index cs (x, y) | x < 0 || x >= columns = Nothing
                  | y < 0 || y >= rows = Nothing
                  | length (cs !! x) <= y = Just Empty
                  | otherwise = Just $ (cs !! x) !! y

  dropCoin :: Board -> Coin -> Int -> Maybe Board
  dropCoin cs coin i | i < 0 || i >= columns = Nothing
                     | length column >= rows = Nothing
                     | otherwise = Just $ (take i cs) ++ [(column ++ [coin])] ++ (drop (i + 1) cs)
                     where
                       column = (cs !! i)
