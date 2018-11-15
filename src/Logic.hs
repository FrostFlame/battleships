module Logic where

import Types
import Util
import WorkWithCLI
import Bot

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
                                        string <- getCoordinateCli
                                        let coord = convertStringToCoordinates string
                                        if validateCoordinate coord then
                                            do
                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                              if hit then
                                                  printHitCli coord 
                                              else
                                                  printMissCli coord

                                              if length newEnemyShips < length enemyShips then
                                                    printSunkCli
                                                    
                                              if length newEnemyShips == 0 then
                                                    return (enemyField, enemyShips)
                         
                                              if hit then
                                                  do
                                                    printFieldCli name newEnemyField newEnemyShips
                                                    turn (newEnemyField, newEnemyShips, name)
                                              else
                                                  return (enemyField, enemyShips)
                                        else
                                            turn (enemyField, enemyShips, name)
                                            
                                            
                                            
turnBot :: (Field, [Ship], String) -> IO (Field, [Ship])
turnBot (enemyField, enemyShips, name) = do
                                        string <- generateCoordinate
                                        let coord = convertStringToCoordinates string
                                        if validateCoordinate coord then
                                            do
                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                              if length newEnemyShips == 0 then
                                                    return (enemyField, enemyShips)
                         
                                              if hit then
                                                  turn (newEnemyField, newEnemyShips, name)
                                              else
                                                  return (enemyField, enemyShips)
                                        else
                                            turn (enemyField, enemyShips, name)