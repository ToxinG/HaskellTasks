{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies (parListChunk, parMap, rdeepseq, using)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, fromList, generate, unsafeIndex)

-- row count, column count, vector of concatenated rows
type Matrix = (Int, Int, Vector Int)

listToMatrix :: [[Int]] -> Matrix
listToMatrix x = do
    let !rows = length x
    let !cols = length (head x)
    (rows, cols, fromList (concat x))
    
transposeMatrix :: Matrix -> Matrix
transposeMatrix (!rows, !cols, !a) = (cols, rows, generate (rows * cols) getElem) where
    getOrigIdx !idx = 
        let (!j, !i) = idx `divMod` rows
            in i * cols + j
    getElem !idx = unsafeIndex a (getOrigIdx idx)

mulMatPar :: Matrix -> Matrix -> [[Int]]
mulMatPar (!ar, !ac, !a) (!btr, !btc, !bt) =
    let !sparkOps = 500000
        idxRes = [[(i, j) | j <- [0 .. btr - 1]] | i <- [0 .. ar - 1]]
        in if ar * ac * btr <= sparkOps
            then map countRow idxRes
            else if ac * btr <= sparkOps
                then let !rowsChunk = sparkOps `div` (ac * btr)
                    in map countRow idxRes `using` parListChunk rowsChunk rdeepseq
                else let !cellsChunk = max 1 (sparkOps `div` ac)
                    in parMap (parListChunk cellsChunk rdeepseq) countRow idxRes
    where       
        countProd !i !j !k = unsafeIndex a (i * ac + k) * unsafeIndex bt (j * btc + k)     
        countCell (!i, !j) = foldl' (+) 0 (map (countProd i j) [0 .. ac - 1])
        countRow = map countCell

multiplicationIntro :: (Matrix -> Matrix -> [[Int]]) -> [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplicationIntro mulImpl a b = if length (head a) /= length b
    then Nothing
    else Just $ mulImpl (listToMatrix a) (transposeMatrix $ listToMatrix b)

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply = multiplicationIntro mulMatPar

main = print $ multiply (replicate 500 [1 .. 500]) (replicate 500 [1 .. 500])
