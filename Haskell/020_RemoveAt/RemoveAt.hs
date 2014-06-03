import Data.List

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []   = (Nothing, [])
removeAt k (x:xs) 
    | k <= 0    = (Nothing, x:xs)
    | k == 1    = (Just x, xs)
    | otherwise = let (e, acc) = removeAt (k-1) xs in (e, x:acc)

removeAt' k = (\(first, second) -> (head second, (first ++ tail second))) . splitAt (k-1)
