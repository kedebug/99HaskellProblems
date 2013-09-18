import Control.Arrow

split [] _ = ([], [])
split x@(y:ys) n 
    | n > 0     = (y:h, s) 
    | otherwise = ([], x) 
  where
    (h, s) = split ys (n-1)

split' xs n = foldr helper ([], []) $ zip [1..] xs
  where
    helper x (l, r) | fst x > n = (l, snd x : r)
                    | otherwise = (snd x : l, r)

split'' (x:xs) n | n > 0 = (:) x . fst &&& snd $ split'' xs (n-1)
split'' xs _ = ([], xs)

split''' xs n = foldl (\(l, r) x -> if (length l < n) then (l++[x], r) else (l, r++[x])) ([], []) xs
