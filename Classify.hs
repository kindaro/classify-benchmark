module Classify where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

classify :: Ord a => Set a -> Set (Set a)
classify = classifyBy (==)

classifyBy :: Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
classifyBy eq = Set.fromList . Map.elems . List.foldl' f Map.empty . Set.toList
  where
    f m x = case List.find (`eq` x) (Map.keys m) of
        Just k  -> Map.insertWith Set.union k (Set.singleton x) m
        Nothing -> Map.insert x (Set.singleton x) m
