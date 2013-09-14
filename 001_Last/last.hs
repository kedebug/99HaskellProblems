myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- const x _ = x
-- const id :: b -> a -> a
myLast' = foldr1 (const id)  
myLast'' = foldr1 (flip const)

myLast''' = head . reverse

myLast'''' x = x !! (length x - 1)
