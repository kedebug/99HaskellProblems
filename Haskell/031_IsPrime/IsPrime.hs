isPrime :: Integral a => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0) . mod n) $ takeWhile (<= nsqrt) candi
  where 
    nsqrt = floor . sqrt $ fromIntegral n
    candi = 2:3:[x + i | x <- [6,12..], i <- [1, -1]]
