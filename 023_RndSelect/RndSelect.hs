import System.Random
import Control.Monad

rnd_select :: [a] -> Int -> IO [a]
rnd_select lst n
    | null lst  = return []
    | n < 0     = error "N must greater than 0"
    | otherwise = do 
        pos <- replicateM n $ 
                getStdRandom $ randomR (0, length lst - 1)
        return [lst !! p | p <- pos]

rnd_select' :: [a] -> Int -> IO [a]
rnd_select' _ 0 = return []
rnd_select' (x:xs) n = do
    r <- randomRIO (0, length xs) 
    if r < n
    then do rest <- rnd_select' xs (n-1)
            return (x:rest)
    else rnd_select' xs n
