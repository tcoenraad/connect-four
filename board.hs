module Board where

  data Coin = Red | Green | Blue | Yellow | Empty deriving (Eq, Show)
  type Column = [Coin]
  type Board = [Column]

  rows :: Int
  rows = 6

  columns :: Int
  columns = 7

  dropCoin :: Board -> Coin -> Int -> Maybe Board
  dropCoin cs coin i | i < 0 || i >= columns = Nothing
                     | length column >= rows = Nothing
                     | otherwise = Just $ (take i cs) ++ [(column ++ [coin])] ++ (drop (i + 1) cs)
                     where
                       column = (cs !! i)
