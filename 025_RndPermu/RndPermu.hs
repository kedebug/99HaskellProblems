import System.Random

rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu (x:xs) = do
    rand <- randomRIO (0, length xs)
    rest <- rndPermu xs
    return $ let (front, tail) = splitAt rand rest
             in front ++ (x:tail)
