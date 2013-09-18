import Data.List

rotate xs n
    | n > len = rotate xs (n-len)
    | n < 0   = rotate xs (n+len)
    | otherwise = let (h,t) = splitAt n xs in t ++ h
  where len = length xs

rotate' xs n = take len . drop (n `mod` len) . cycle $ xs
  where len = length xs
