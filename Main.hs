{-# language BlockArguments #-}

module Main where

import Data.Function
import qualified Data.Set as Set
import qualified Data.List as List
import Test.QuickCheck
import Criterion
import Criterion.Main

import qualified Classify
import qualified Fischer


classify' :: Ord a => (a -> a -> Bool) -> [a] -> [[a]]
classify' eq = Set.toList . Set.map (Set.toList) . Classify.classifyBy eq . Set.fromDistinctAscList

equivalence :: Int -> Int -> Bool
equivalence = (==) `on` (`mod` 3)

prepareList = List.nub . List.sort

setupEnv = do
    let xs  = [1.. 10^4]
    return xs

main = do
    quickCheck \xs -> let xs' = prepareList xs
                      in classify' equivalence xs' == Fischer.equivalenceClasses equivalence xs'
    defaultMain
        [ env setupEnv \xs -> bgroup ""
            [ bench "classify" $ nf (classify'                  equivalence) xs
            , bench "fischer"  $ nf (Fischer.equivalenceClasses equivalence) xs
            ]
        ]
