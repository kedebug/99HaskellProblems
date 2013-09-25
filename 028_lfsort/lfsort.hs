import Data.List

lsort :: [[a]] -> [[a]]
lsort = sortBy (\l r -> compare (length l) (length r))

lfsort :: [[a]] -> [[a]]
lfsort lst = concat group
  where
    group = lsort . groupBy (\l r -> length l == length r) . lsort $ lst
