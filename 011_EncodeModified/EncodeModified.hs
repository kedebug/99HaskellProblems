import Encode
import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
  where 
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

encodeModified' xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]
