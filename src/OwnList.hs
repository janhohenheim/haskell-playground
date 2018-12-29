module OwnList
( OwnList(..)
, someOwnList
) where

infixr 5 :-:
data OwnList a = Empty | a :-: (OwnList a) deriving (Show, Read, Eq, Ord)

someOwnList :: OwnList Int
someOwnList = 3 :-: 4 :-: 5 :-: Empty
