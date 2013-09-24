import Data.List

combin :: Int -> [a] -> [[a]]
combin 0 _ = return []
combin n xs = do
    y:ys <- tails xs
    rest <- combin (n-1) ys
    return (y:rest)

combin' :: Int -> [a] -> [[a]]
combin' 0 _ = [[]]
combin' _ [] = []
combin' n (x:xs) = xstart ++ rest
  where
    xstart = [x:ys | ys <- combin' (n-1) xs]
    rest = if n <= length xs then combin' n xs else []

combin'' :: Int -> [a] -> [[a]]
combin'' 0 _ = [[]]
combin'' _ [] = []
combin'' n (x:xs) = (map (x:) (combin'' (n-1) xs)) ++ (combin'' n xs)

