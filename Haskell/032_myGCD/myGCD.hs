mygcd :: Integral a => a -> a -> a
mygcd a b 
    | b < 0 = mygcd a (-b)
    | a < 0 = mygcd (-a) b
    | b == 0 = a 
    | otherwise = mygcd b (mod a b)

mygcd' :: Integral a => a -> a -> a
mygcd' a b
    | b == 0 = abs a
    | otherwise = mygcd' b (mod a b)
