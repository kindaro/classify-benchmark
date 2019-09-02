module Classify where

import qualified Data.List as List

classify :: Eq a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy eq = List.foldl' f [ ]
  where
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y
