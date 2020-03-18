module Main where

import Data.List
import Control.Parallel.Strategies

mulVec :: [Int] -> [Int] -> Int
u `mulVec` v = sum (zipWith (*) u v)

mulMat :: [[Int]] -> [[Int]] -> [[Int]]
a `mulMat` b = [[u `mulVec` v | v <- bt] | u <- a] where
    bt = transpose b

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply a b = if length a /= length (transpose b)
    then Nothing
    else Just ((a `mulMat` b) `using` strat) where
        strat = parList rdeepseq
    
main = print $ multiply (replicate 20 [1 .. 2000]) (replicate 2000 [1 .. 20])
