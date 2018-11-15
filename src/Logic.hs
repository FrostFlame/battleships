module Logic where

import Types
import Util
import WorkWithCLI

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


turn :: (Field, [Ship], String) -> IO (Field, [Ship])
turn (enemyField, enemyShips, name) = do
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
                                                                    printFieldCli name newEnemyField newEnemyShips
                                                                    turn (newEnemyField, newEnemyShips, name)
                                                              else
                                                                  return (enemyField, enemyShips)
                                                        else
                                                            return (enemyField, enemyShips)