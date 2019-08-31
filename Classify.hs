{-# language ScopedTypeVariables #-}

module Classify where

import qualified Data.List as List

classify :: Ord a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: forall a. Ord a => (a -> a -> Bool) -> [a] -> [[a]]
classifyBy eq = List.foldl' f [ ]
  where
    f :: [[a]] -> a -> [[a]]
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y
