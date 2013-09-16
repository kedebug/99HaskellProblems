data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs) 

flatten' :: NestedList a -> [a]
flatten' (Elem x)  = [x]
flatten' (List xs) = foldr (++) [] $ map flatten' xs

flatten'' :: NestedList a -> [a]
flatten'' a = f a []
  where
    f (Elem x)      list = x:list
    f (List [])     list = list
    f (List (x:xs)) list = f x (f (List xs) list)

flatten''' :: NestedList a -> [a]
flatten''' (Elem x)  = [x]
flatten''' (List xs) = concatMap flatten''' xs
