import Data.List
import Control.Applicative

dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

dupli' = concatMap (\x -> [x,x])

dupli'' list = concat [[x,x] | x <- list]

dupli''' = foldr ((.) <$> (:) <*> (:)) []
