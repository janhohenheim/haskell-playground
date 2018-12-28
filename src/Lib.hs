module Lib
    ( someFunc
    ) where

import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map
import Geometry.Cube

someFunc :: IO ()
someFunc = 
    let phoneMap = Map.fromList phoneNumbers
        someNumber = case Map.lookup "betty" phoneMap of
            Just number -> "The number is" ++ number
            Nothing -> "No number found"
    in  putStrLn $ show $ someNumber

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

phoneNumbers :: [(String, String)]
phoneNumbers = [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
