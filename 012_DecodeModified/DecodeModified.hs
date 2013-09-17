import Data.List

data ListEncoded a = Single a | Multiple Int a deriving (Show)

decodeModified = concatMap decodeHelper 
  where
    decodeHelper (Single x)     = [x]
    decodeHelper (Multiple n x) = replicate n x

toTuple :: ListEncoded a -> (Int, a)
toTuple (Single x)     = (1, x)
toTuple (Multiple n x) = (n, x)

decodeModified' = concatMap (uncurry replicate . toTuple)

decodeModified'' = foldl decodeHelper []
  where
    decodeHelper acc e = case e of
        Single x     -> acc ++ [x]
        Multiple n x -> acc ++ replicate n x 
