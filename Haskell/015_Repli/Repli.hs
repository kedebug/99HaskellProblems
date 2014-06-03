import Data.List

repli xs n = concatMap (take n . repeat) xs

repli' = flip $ concatMap . replicate

repli'' xs n = foldl (\acc x -> acc ++ helper n x) [] xs
  where
    helper 0 _ = []
    helper n x = x : helper (n-1) x

repli''' [] _     = []
repli''' (x:xs) n = foldr (const (x:)) (repli''' xs n) [1..n] -- cool
