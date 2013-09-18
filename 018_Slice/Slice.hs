slice xs i j = map fst 
             $ filter (\(_, x) -> x >= i && x <= j) 
             $ zip xs [1..j]

slice' xs i j = [x | (x, k) <- zip xs [1..j], k >= i]

slice'' []     _ _ = []
slice'' (x:xs) i j
    | i > 1     = slice'' xs (i-1) (j-1)
    | j < 1     = []
    | otherwise = x : slice'' xs (i-1) (j-1)

slice''' xs i j 
    | i > 0 = take (j-i+1) $ drop (i-1) xs
    | otherwise = take j xs
