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
import qualified Projection

equivalence :: Int -> Int -> Int -> Bool
equivalence n = (==) `on` (`mod` n)

setupEnv n = do
    xs <- fmap (take n) . generate $ (infiniteList @Int)
    return xs

installation n k = env (setupEnv (2^n)) \xs -> bgroup message
  [ bench "classify" $ nf (Classify.classifyBy (equivalence k)) xs
  , bench "fischer" $ nf (Fischer.equivalenceClasses (equivalence k)) xs
  , bench "projection" $ nf (Projection.classifyByProjection (`mod` k)) xs
  ]
  where
    message = "list length 2^" ++ show n ++ " = " ++ show (2^n)
      ++ ", " ++ show k ++ " classes "

normalizeResult = List.sortBy (compare `on` head) . fmap List.sort

(=->) f g = \x -> f x === g x

main = do
    quickCheck $   (normalizeResult . Classify.classifyBy        (equivalence 3))
               =-> (normalizeResult . Fischer.equivalenceClasses (equivalence 3))
    quickCheck $   (normalizeResult . Classify.classifyBy             (equivalence 3))
               =-> (normalizeResult . Projection.classifyByProjection (`mod` 3))
    quickCheck $   (normalizeResult . Fischer.equivalenceClasses      (equivalence 3))
               =-> (normalizeResult . Projection.classifyByProjection (`mod` 3))
    defaultMain $ fmap (uncurry installation) [ (lgN, 2^lgK) | lgN <- [10, 12..16], lgK <- [1, 3..15], lgN > lgK]
