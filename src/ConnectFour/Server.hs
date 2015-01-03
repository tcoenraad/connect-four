module ConnectFour.Server where

  type Player = String

  queue :: Player -> [Player] -> Int -> Either [Player] [Player]
  queue player group i | length group' == i = Left group'
                           | otherwise = Right group'
                    where
                      group' = group ++ [player]
