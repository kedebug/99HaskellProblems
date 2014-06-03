import Data.List

dropEvery [] _ = []
dropEvery xs n = fst $ foldl (\(acc,l) x -> if l == n then (acc,1) else (acc++[x],l+1)) ([],1) xs

dropEvery' xs n = helper xs n
  where
    helper [] _     = []
    helper (x:xs) 1 = helper xs n
    helper (x:xs) k = x : helper xs (k-1)

dropEvery'' [] _ = []
dropEvery'' xs n = take (n-1) xs ++ dropEvery'' (drop n xs) n

dropEvery''' xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) $ zip xs [1..]

dropEvery'''' xs n = concat (split' n xs)
  where
    split' _ [] = []
    split' n xs = fst splitted : split' n (safetail $ snd splitted)
      where
        splitted = splitAt (n-1) xs
        safetail xs | null xs   = []
                    | otherwise = tail xs
