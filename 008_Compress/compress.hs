import Data.List

compress :: (Eq a) => [a] -> [a]
compress xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

compress'' :: (Eq a) => [a] -> [a]
compress'' (x:ys@(y:_))
    | x == y    = compress'' ys
    | otherwise = x : compress'' ys
compress'' ys = ys

compress''' :: (Eq a) => [a] -> [a]
compress''' = foldr skipDups []
  where
    skipDups x [] = [x]
    skipDups x (y:ys) 
        | x == y    = y:ys
        | otherwise = x:y:ys
