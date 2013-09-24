insertAt :: a -> [a] -> Int -> [a] 
insertAt x xs n = first ++ [x] ++ second
  where
    (first, second) = splitAt (n-1) xs

insertAt' :: a -> [a] -> Int -> [a]
insertAt' x xs 1 = x:xs
insertAt' x (y:ys) n = y : insertAt' x ys (n-1) 
