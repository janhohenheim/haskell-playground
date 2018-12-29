module Lib
    ( someFunc
    ) where

import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
import Geometry.Shapes
import Person
import Day

someFunc :: IO ()
someFunc = putStrLn $ show $ ([minBound .. maxBound] :: [Day])


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

type PhoneBook = [(String, String)]
phoneNumbers :: PhoneBook
phoneNumbers = [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]

person :: Person
person = Person 
    { firstName = "Jan Nils"
    , lastName = "Ferner"
    , age = 21
    , height = 1.84
    , phoneNumber = "012 345 67 89"
    , flavor = "Strawberry"
    }
    