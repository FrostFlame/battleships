module Types where

type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[Bool]]
type Player = String

fieldSize = 10
botname = "bot"
shipLengthList = [1,1,1,1,2,2,2,3,3,4]
shipCount = 10