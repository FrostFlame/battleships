module Util where

import Data.Char
import Data.List (permutations)
import Types

convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0'), (ord y) - (ord '0'))
convertStringToCoordinates _ = (-1, -1)



splitCoordinatesInString :: String -> [String]
splitCoordinatesInString [] = [[]]
splitCoordinatesInString (x:xs) = if x == ';' then
                                      [] : splitCoordinatesInString xs
                                  else
                                      (x : head (splitCoordinatesInString xs)) : tail (splitCoordinatesInString xs)








convertFieldToString :: Field -> [Ship] -> Coordinate -> Int -> String
convertFieldToString field ships coordinate x
        | fst coordinate <= fieldSize
          && snd coordinate <= fieldSize = if select (fst coordinate) (select (snd coordinate) field) == True then
                                               if or [coordinate == coord | ship <- ships, coord <- ship] then 'o' : convertFieldToString field ships (fst coordinate + 1, snd coordinate) x
                                                   else 'x' : convertFieldToString field ships (fst coordinate + 1, snd coordinate) x
                                           else ' ' : convertFieldToString field ships (fst coordinate + 1, snd coordinate) x
                                        
        | snd coordinate <= fieldSize && x < 9 = [intToDigit(x)] ++ "\n" ++ [intToDigit(x + 1)] ++ convertFieldToString field ships (1, snd coordinate + 1) (x + 1)
        | snd coordinate <= fieldSize = [intToDigit(x)] ++ "\n" ++ convertFieldToString field ships (1, snd coordinate + 1) (x + 1)
        | otherwise = []


convertMyFieldToString :: Field -> [Ship] -> Coordinate -> Int -> String
convertMyFieldToString field ships coordinate x
        | fst coordinate <= fieldSize - 1
          && snd coordinate <= fieldSize - 1 = if or [coordinate == coord | ship <- ships, coord <- ship] then 'x' : convertMyFieldToString field ships (fst coordinate + 1, snd coordinate) x
                                                   else '-' : convertMyFieldToString field ships (fst coordinate + 1, snd coordinate) x
                                        
        | snd coordinate <= fieldSize - 1 && x < 9 = [intToDigit(x)] ++ "\n" ++ [intToDigit(x + 1)] ++ convertMyFieldToString field ships (0, snd coordinate + 1) (x + 1)
        | snd coordinate <= fieldSize - 1 = [intToDigit(x)] ++ "\n" ++ convertMyFieldToString field ships (1, snd coordinate + 1) (x + 1)
        | otherwise = []

        
        
validateCoordinate :: Coordinate -> Bool
validateCoordinate coord = and [ fst coord >= 0,
                                 snd coord >= 0,
                                 fst coord < fieldSize,
                                 snd coord < fieldSize
                               ]
                               
     
getAround  :: Coordinate -> [Coordinate]
getAround coord = [((fst coord) -1 , (snd coord)-1),  ((fst coord) -1 , (snd coord)  ),
                   ((fst coord) -1 , (snd coord)+1),  ((fst coord)    , (snd coord)-1),
                   ((fst coord)    , (snd coord)  ),  ((fst coord)    , (snd coord)+1),
                   ((fst coord) +1 , (snd coord)-1),  ((fst coord) +1 , (snd coord)  ),
                   ((fst coord) +1 , (snd coord)+1)]
     
validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool
validateShipCoordinates placedShips ship shipLength
    | length ship /= shipLength = False
    | or [coord1 == coord2 | ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False
    | or [coord1 == around | coord1 <- ship, ship2 <- placedShips, coord2 <- ship2, around <- getAround coord2] = False
    | not (and [validateCoordinate coord | coord <- ship]) = False
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship])
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship])
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | otherwise = False
    

select :: Int -> [a] -> a
select n xs = head (drop (n-1) (take n xs))

replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs
    
    
    
