module Logic where

import Types
import Util
import WorkWithCLI
import Bot
import Control.Monad

markShot :: Field -> Int -> Int -> CellState -> Field
markShot field x y state = replace x field (replace y (field !! x) state)


removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs


checkShipDestroyed :: Field -> Ship -> Ship
checkShipDestroyed field ship = if and [select (fst coord) (select (snd coord) field) == Hit | coord <- ship] == False then
                                                   ship -- Чек иф селект воркс ас интендед
                                               else
                                                   []   -- Hit and sunk


fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate =  let flag = if or [coordinate == coord | ship <- enemyShips, coord <- ship] then True else False
                                                state = if flag then Hit else Miss
                                            in (markShot enemyField (snd coordinate) (fst coordinate) state,
                                            removeDestroyedShips [checkShipDestroyed enemyField ship | ship <- enemyShips], flag)


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

                                              when (length newEnemyShips < length enemyShips) printSunkCli
                                              if (length newEnemyShips == 0) then
                                                  return (enemyField, enemyShips)
                                              else
                                                do
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
                                        coord <- generateCoordinate enemyField
                                        if validateCoordinate coord then
                                            do
                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                              if hit then
                                                  printHitCli coord 
                                              else
                                                  printMissCli coord
                                              
                                              when (length newEnemyShips < length enemyShips) printSunkCli
                                              if (length newEnemyShips == 0) then
                                                  return (enemyField, enemyShips)
                                              else
                                                do
                                                  if hit then
                                                      do
                                                        turnBot (newEnemyField, newEnemyShips, name)
                                                  else
                                                      return (enemyField, enemyShips)
                                        else
                                            turnBot (enemyField, enemyShips, name)

