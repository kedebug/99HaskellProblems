myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- same, with accumulator
myReverse' :: [a] -> [a]
myReverse' list =
    reverse' list []
  where
    reverse' [] revList     = revList
    reverse' (x:xs) revList = reverse' xs (x:revList)

myReverse'' = foldl (flip (:)) []
