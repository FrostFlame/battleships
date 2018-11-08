import Data.Char
import Data.List (permutations)
import System.Random


type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[Bool]]
type Player = String


fieldSize = 10
botname = "bot"
shipLengthList = [1,1,1,1,2,2,2,3,3,4]
shipCount = 10


select :: Int -> [a] -> a
select n xs = head (drop (n-1) (take n xs))


replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs


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


printField :: String -> Field -> [Ship] -> IO ()
printField playerName field ships = do
                                      putStrLn (playerName ++ "'s field:")
                                      putStrLn ("#0123456789#\n0" ++ convertFieldToString field ships (1, 1) 0 ++ "#0123456789#")
                                      putStrLn ""


markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (select x field) True)


removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs


checkShipDestroyed :: Field -> Ship -> Coordinate -> (Ship, Bool)
checkShipDestroyed field ship coordinate = if or [coordinate == coord | coord <- ship] == False then do
                                               (ship, False)    -- Miss
                                           else do
                                               if and [select (fst coord) (select (snd coord) field) == True | coord <- ship, coord /= coordinate] == False then
                                                   (ship, True) -- Hit, but not sunk
                                               else
                                                   ([], True)   -- Hit and sunk



fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate = (markShot enemyField (snd coordinate) (fst coordinate),
                                            removeDestroyedShips [fst (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips],
                                            or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips])



fireWithEveryShip :: (Field, [Ship], String) -> IO (Field, [Ship])
fireWithEveryShip (enemyField, enemyShips, name) = do
                                                        putStrLn ("Enter the coordinates to fire shot")
                                                        string <- getLine
                                                        let coord = convertStringToCoordinates string
                                                        if validateCoordinate coord then
                                                            do
                                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                                              if hit then
                                                                  putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Hit")
                                                              else
                                                                  putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Miss")

                                                              if length newEnemyShips < length enemyShips then
                                                                  do
                                                                    putStrLn "You sunk my battleship!"
                                                                    return (enemyField, enemyShips)
                                                              else
                                                                  return (enemyField, enemyShips)
                                                              if hit then
                                                                  do
                                                                    printField name newEnemyField newEnemyShips
                                                                    fireWithEveryShip (newEnemyField, newEnemyShips, name)
                                                              else
                                                                  return (enemyField, enemyShips)
                                                        else
                                                            return (enemyField, enemyShips)


game :: [String] -> [Field] -> [[Ship]] -> IO ()
game names fields ships = do
                            putStrLn ("\n" ++ head names ++ "'s turn")
                            printField (last names) (last fields) (last ships)
                            (newField, newShipList) <- fireWithEveryShip (last fields, last ships, last names)
                            if length newShipList == 0 then
                                do
                                  putStrLn ("\n" ++ head names ++ " won!\n")
                                  printField (last names) newField newShipList
                                  printField (head names) (head fields) (head ships)
                            else
                                game [last names, head names] [newField, head fields] [newShipList, head ships]


generateShip :: [Coordinate] -> Int -> Int -> IO Ship
generateShip [] len direction = do
                          f <- newStdGen
                          g <- newStdGen
                          let coord = (fst(randomR (0, 10) f) :: Int, fst(randomR (0, 10) g) :: Int)
                          generateShip ([coord]) (len-1) direction
generateShip list 0 direction | direction == 0 = do
                                                return (list ++ [(fst(last list), snd(last list) + 1)])
                              | direction == 1 = do
                                                return (list ++ [(fst(last list) + 1, snd(last list))])
generateShip list len direction | direction == 0 = generateShip (list ++ [(fst(last list), snd(last list) + 1)]) (len - 1) direction
                                | direction == 1 = generateShip (list ++ [(fst(last list) + 1, snd(last list))]) (len - 1) direction


getCoordsFromCli :: Int -> IO Ship
getCoordsFromCli len= do
                    putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                    string <- getLine
                    let stringCoords = splitCoordinatesInString string
                    let coords = map convertStringToCoordinates stringCoords
                    return coords

inputShip :: [Ship] -> Int -> String -> IO Ship
inputShip placedShips len player = do
                                    coords <- if (player == botname)
                                      then do
                                        r <- randomRIO(0, 1)
                                        generateShip [] len r
                                      else getCoordsFromCli len
                                
                              
                                    --to do validate ship location
                                    if validateShipCoordinates placedShips coords len then
                                       return coords
                                    else
                                       inputShip placedShips len player


inputShips :: Int -> [Ship] -> String -> IO [Ship]
inputShips listSize placedShips player = if listSize < shipCount then
                                      do
                                        ship <- inputShip placedShips (shipLengthList !! listSize) player
                                        shipList <- inputShips (listSize + 1) (ship : placedShips) player
                                        return (ship : shipList)
                                  else
                                      return []


-- The entry point of the program
main :: IO ()
main = do
         putStrLn "What is the name of the player?"
         name <- getLine
         let names = [name, botname]
         putStrLn (name ++ ", enter your ships")
         shipsPlayer <- inputShips 0 [] name
         --to do add computer ships
         shipsComputer <- inputShips 0 [] botname

         -- game names [initField, initField] [shipsPlayer, shipsComputer]
         putStrLn ""