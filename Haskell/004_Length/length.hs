myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = myLength xs + 1

myLength' :: [a] -> Int
myLength' list = 
    length_acc list 0
  where
    length_acc [] n     = n
    length_acc (_:xs) n = length_acc xs (n+1)

myLength''  = foldl (\n _ -> n + 1) 0
myLength''' = foldr (\_ n -> n + 1) 0

{- 
myLength = foldr (const (+1)) 0
myLength = foldr ((+) . const 1) 0
myLength = foldl (const . (+1)) 0
myLength = fst . last . zip [1..]
-}
