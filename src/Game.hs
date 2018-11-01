
fieldSize = 10
botname = "bot"
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


getCoordsFromCli :: IO Ship
getCoordsFromCli = do
                    putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                    string <- getLine
                    stringCoords <- splitCoordinatesInString string
                    coords <- map convertStringToCoordinates stringCoords
                    return coords

inputShip :: [Ship] -> Int -> String -> IO Ship
inputShip placedShips len player = do
                                    if player == botname then
                                    --to do generate ship for bot
                                    coords <- generateInputShip
                                    else
                                    coords <-getCoordsFromCli
                                
                              
                                    --to do validate ship location
                                    if validateShip placedShips coords len then
                                        return coords
                                    else
                                        inputShip placedShips len


inputShips :: Int -> [Ship] -> String -> IO [Ship]
inputShips listSize placedShips player = if listSize < shipCount then
                                      do
                                        ship <- inputShip placedShips (shipLengthList !! listSize) player
                                        shipList <- inputShips (listSize + 1) (ship : placedShips) player
                                        return (ship : shipList)
                                  else
                                      return []

                                      
main :: IO ()
main = do
         putStrLn "What is the name of the player?"
         name <- getLine
         names = [name, botname]
         putStrLn (name ++ ", enter your ships")
         shipsPlayer <- inputShips 0 [] name
         --to do add computer ships
         shipsComputer <- inputShips 0 [] botname

         game names [initField, initField] [shipsPlayer, shipsComputer]
         

         
