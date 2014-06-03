range :: (Enum a, Ord a) => a -> a -> [a]
range a b | (a == b)  = [a] 
          | (a < b)   = a : range (succ a) b
          | otherwise = a : range (pred a) b
