{-# language BlockArguments
           , TypeApplications
           , NoMonomorphismRestriction
  #-}

module Main where

import Data.Function
import qualified Data.List as List
import Test.QuickCheck
import Criterion
import Criterion.Main

import qualified Classify
import qualified Fischer

equivalence :: Int -> Int -> Int -> Bool
equivalence n = (==) `on` (`mod` n)

setupEnv n = do
    xs <- fmap (take n) . generate $ (infiniteList @Int)
    return xs

installation n k = env (setupEnv (2^n)) \xs -> bgroup message
  [ bench1 "classify" Classify.classifyBy xs
  , bench1 "fischer" Fischer.equivalenceClasses xs
  ]
  where
    message = "list length 2^" ++ show n ++ " = " ++ show (2^n)
      ++ ", " ++ show k ++ " classes "
    bench1 name f x = bench name  $ nf (f (equivalence k)) x

normalizeResult = List.sortBy (compare `on` head) . fmap List.sort

(=->) f g = \x -> f x === g x

main = do
    quickCheck $   (normalizeResult . Classify.classifyBy        (equivalence 3))
               =-> (normalizeResult . Fischer.equivalenceClasses (equivalence 3))
    defaultMain $ fmap (uncurry installation) [ (lgN, 2^lgK) | lgN <- [10, 12..20], lgK <- [1, 3..9]]
