myLast :: [a] -> a
myLast [] = error "No end for empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- const :: a -> b -> a
-- const x _ = x

myLast' :: [a] -> a
myLast' = foldr1 (const id)


