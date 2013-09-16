import Data.List
import Control.Applicative

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group 

encode' xs = [(length x, head x) | x <- group xs]

encode'' :: (Eq a) => [a] -> [(Int, a)]
encode'' = map ((,) <$> length <*> head) . group
