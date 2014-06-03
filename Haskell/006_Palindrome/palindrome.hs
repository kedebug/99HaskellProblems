import Control.Applicative

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == (reverse xs)

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
  where
    input = zip xs $ reverse xs

isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = (==) <*> reverse

isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' xs = f [] xs xs
  where
    f rev (x:xs) (_:_:ys) = f (x:rev) xs ys
    f rev (x:xs) [_]      = rev == xs
    f rev xs []           = rev == xs
