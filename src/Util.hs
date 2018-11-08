
import Data.Char
import Data.List (permutations)

convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
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
                                        
        | snd coordinate <= fieldSize && x < 9 = [intToDigit(x)] ++ "\n" ++ [intToDigit(x+1)] ++ convertFieldToString field ships (1, snd coordinate + 1) (x+1)
        | snd coordinate <= fieldSize = [intToDigit(x)] ++ "\n" ++ convertFieldToString field ships (1, snd coordinate + 1) (x+1)
        | otherwise = []
        
        
        
validateCoordinate :: Coordinate -> Bool
validateCoordinate coord = and [ fst coord >= 1,
                                 snd coord >= 1,
                                 fst coord <= fieldSize,
                                 snd coord <= fieldSize
                               ]
                               
                               
validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool
validateShipCoordinates placedShips ship shipLength
    | length ship /= shipLength = False
    | or [coord1 == coord2 | ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False
    | not (and [validateCoordinate coord | coord <- ship]) = False
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship])
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship])
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | otherwise = False
    
    
    
    
    
