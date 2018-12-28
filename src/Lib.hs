module Lib
    ( someFunc
    ) where

import Data.List
import Data.Function
import Data.Char


someFunc :: IO ()
someFunc = putStrLn $ show $ caesarEncode 4 "Hi there!"

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

howManyElementsDoesItTakeForTheSumOfTheRootsOfAllNaturalNumbersToExceedANumber :: (Floating a, Ord a, Enum a) => a -> Int
howManyElementsDoesItTakeForTheSumOfTheRootsOfAllNaturalNumbersToExceedANumber x = (+) 1 $ length $ takeWhile (<x) $ scanl1 (+) $ map sqrt [1..]

makeAllNegative :: (Num a) => [a] -> [a]
makeAllNegative = map $ negate . abs

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

countUnique :: (Eq a) => [a] -> Int
countUnique = length . nub

groupByLength :: (Num a) => [[a]] -> [[a]]
groupByLength = sortBy (compare `on` length)

caesarEncode :: Int -> String -> String
caesarEncode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted