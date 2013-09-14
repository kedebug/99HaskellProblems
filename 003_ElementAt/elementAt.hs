elementAt :: [a] -> Int -> a
elementAt list n = list !! (n-1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1  = x
elementAt' [] _     = error "Index out of bound"
elementAt' (_:xs) k 
    | k < 1         = error "Index out of bound"
    | otherwise     = elementAt' xs (k-1)

elementAt'' :: [a] -> Int -> a
elementAt'' xs n = head $ foldr ($) xs 
                        $ replicate (n-1) tail

