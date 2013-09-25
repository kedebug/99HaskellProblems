combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)] 
combination _ [] = []
combination n (x:xs) = left ++ right
  where
    left  = [(x:selected, remaining) | (selected, remaining) <- combination (n-1) xs]
    right = [(selected, x:remaining) | (selected, remaining) <- combination n xs] 

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (x:xs) lst = do
    (selected, remaining) <- combination x lst
    rest <- group xs remaining
    return $ selected : rest 
