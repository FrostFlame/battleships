module Types where

type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[CellState]]
type Player = String

data CellState = Empty | Miss | Hit | Dead deriving(Eq, Show)

fieldSize :: Int
fieldSize = 10


botname = "bot"


shipLengthList :: [Int]
shipLengthList = [1,1,1,1,2,2,2,3,3,4]

shipCount :: Int
shipCount = 10