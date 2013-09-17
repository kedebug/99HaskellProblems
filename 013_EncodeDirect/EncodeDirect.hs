data ListItem a = Single a | Multiple Int a deriving (Show)

encode :: (Eq a) => [a] -> [(Int,a)]
encode = foldr helper []
  where 
    helper x []     = [(1,x)]
    helper x (y@(a,b):ys)
        | x == b    = (a+1,b):ys
        | otherwise = (1,x):y:ys

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect = map helper . encode 
  where
    helper (1,x) = Single x
    helper (n,x) = Multiple n x

encodeDirect' :: (Eq a) => [a] -> [ListItem a]
encodeDirect' (x:xs) = encode' 1 x xs
  where
    encode' n x [] = [encodeElem n x]
    encode' n x (y:ys) | x == y = encode' (n+1) x ys
                       | otherwise = (encodeElem n x) : (encode' 1 y ys)
    encodeElem 1 x = Single x
    encodeElem n x = Multiple n x
