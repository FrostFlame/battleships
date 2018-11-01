
fieldSize = 10
shipLengthList = [1,1,1,1,2,2,2,3,3,4]
shipCount = 10 

initField :: Field
initField = take fieldSize (repeat (take fieldSize (repeat False)))


convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertStringToCoordinates _ = (-1, -1)



splitCoordinatesInString :: String -> [String]
splitCoordinatesInString [] = [[]]
splitCoordinatesInString (x:xs) = if x == ';' then
                                      [] : splitCoordinatesInString xs
                                  else
                                      (x : head (splitCoordinatesInString xs)) : tail (splitCoordinatesInString xs)




inputShip :: [Ship] -> Int -> IO Ship
inputShip placedShips len = do
                              putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                              string <- getLine
                              let stringCoords = splitCoordinatesInString string
                              let coords = map convertStringToCoordinates stringCoords
                              --to do validate ship location
                              if validateShip placedShips coords len then
                                  return coords
                              else
                                  inputShip placedShips len


inputShips :: Int -> [Ship] -> IO [Ship]
inputShips listSize placedShips = if listSize < shipCount then
                                      do
                                        ship <- inputShip placedShips (shipLengthList !! listSize)
                                        shipList <- inputShips (listSize + 1) (ship : placedShips)
                                        return (ship : shipList)
                                  else
                                      return []

                                      
main :: IO ()
main = do
         putStrLn "What is the name of the player?"
         name <- getLine
         putStrLn (name ++ ", enter your ships")
         shipsPlayer <- inputShips 0 []

         shipsComputer <- inputShips 0 []

         -- to do method game
         game names [initField, initField] [shipsPlayer, shipsComputer]
         
         
         
