{-# language BlockArguments
           , TypeApplications
           , NoMonomorphismRestriction
  #-}

module Main where

import Data.Function
import qualified Data.List as List
import Test.QuickCheck hiding ((===))
import Criterion
import Criterion.Main

import qualified Classify
import qualified Fischer

equivalence :: Int -> Int -> Int -> Bool
equivalence n = (==) `on` (`mod` n)

setupEnv n = do
    xs <- fmap (take n) . generate $ (infiniteList @Int)
    return xs

normalizeResult = List.sortBy (compare `on` head) . fmap List.sort

(===) f g = \x -> f x == g x

main = do
    quickCheck $   (normalizeResult . Classify.classifyBy        (equivalence 3))
               === (normalizeResult . Fischer.equivalenceClasses (equivalence 3))
    defaultMain
        [ env (setupEnv (2^22)) \xs -> bgroup "x"
            [ bench "classify" $ nf (Classify.classifyBy        (equivalence 3)) xs
            , bench "fischer"  $ nf (Fischer.equivalenceClasses (equivalence 3)) xs
            ]
        , env (setupEnv (2^16)) \xs -> bgroup "y"
            [ bench "classify" $ nf (Classify.classifyBy        (equivalence (2^10))) xs
            , bench "fischer"  $ nf (Fischer.equivalenceClasses (equivalence (2^10))) xs
            ]
        , env (setupEnv (2^12)) \xs -> bgroup "z"
            [ bench "classify" $ nf (Classify.classifyBy        (==)) xs
            , bench "fischer"  $ nf (Fischer.equivalenceClasses (==)) xs
            ]
        ]
