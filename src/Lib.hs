module Lib
    ( someFunc
    ) where

import qualified Data.Map as Map
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
import Geometry.Shapes
import Person
import Day
import Locker
import SomeFunctions
import OwnList

someFunc :: IO ()
someFunc = putStrLn $ show $ someOwnList
