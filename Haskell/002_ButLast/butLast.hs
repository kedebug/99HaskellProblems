myButLast :: [a] -> a
myButLast [x,_]  = x
myButLast (x:xs) = myButLast xs

myButLast' = last . init

myButLast'' = (!! 1) . reverse 
myButLast''' x = reverse x !! 1

myButLast'''' = head . tail . reverse
