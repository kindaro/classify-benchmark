module Fischer where

import Data.List

splitOffFirstGroup :: (a -> a -> Bool) -> [a] -> ([a],[a])
splitOffFirstGroup equal xs@(x:_) = partition (equal x) xs
splitOffFirstGroup _     []       = ([],[])

equivalenceClasses _     [] = []
equivalenceClasses equal xs = let (fg,rst) = splitOffFirstGroup equal xs
                              in fg : equivalenceClasses equal rst


