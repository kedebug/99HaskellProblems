import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n to = helper n [1..to]
  where
    helper 0 _  = return []
    helper _ [] = error "Too few elements to choose from"
    helper n xs = do
        r <- randomRIO (0, length xs - 1)
        let remaining = take r xs ++ drop (r+1) xs
        rest <- helper (n-1) remaining
        return ((xs!!r) : rest)
