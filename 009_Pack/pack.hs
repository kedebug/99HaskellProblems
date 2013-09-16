import Data.List

pack :: (Eq a) => [a] -> [[a]]
pack = foldr p' []
  where
    p' x [] = [[x]]
    p' x (y:ys)
        | x == head y = (x:y):ys
        | otherwise   = [x]:y:ys

pack' [] = []
pack' (x:xs) = let (first,rest) = span (==x) xs
                in (x:first) : pack' rest

pack'' [] = []
pack'' (x:xs) = (x:reps) : pack'' rest 
  where
    (reps,rest) = maybe (xs,[]) f (findIndex (/=x) xs)
      where
        f i = splitAt i xs
