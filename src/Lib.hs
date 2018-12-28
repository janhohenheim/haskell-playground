module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn $ show $ howManyElementsDoesItTakeForTheSumOfTheRootsOfAllNaturalNumbersToExceedANumber 1000

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

howManyElementsDoesItTakeForTheSumOfTheRootsOfAllNaturalNumbersToExceedANumber :: (Floating a, Ord a, Enum a) => a -> Int
howManyElementsDoesItTakeForTheSumOfTheRootsOfAllNaturalNumbersToExceedANumber x = (+) 1 $ length $ takeWhile (<x) $ scanl1 (+) $ map sqrt [1..]
