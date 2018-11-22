module Logic where

import Types
import Util
import WorkWithCLI
import Bot
import Control.Monad

markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (field !! x) True)


removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs


checkShipDestroyed :: Field -> Ship -> Coordinate -> (Ship, Bool)
checkShipDestroyed field ship coordinate = if or [coordinate == coord | coord <- ship] == False then do
                                               (ship, False)    -- Miss
                                           else do
                                               if and [select (fst coord) (select (snd coord) field) == True | coord <- ship, coord /= coordinate] == False then
                                                   (ship, True) -- Чек иф селект воркс ас интендед
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

                                              when (length newEnemyShips < length enemyShips) printSunkCli
                                              if (length newEnemyShips == 0) then
                                                  return (enemyField, enemyShips)
                                              else
                                                do
                                                  if hit then
                                                      do
                                                        -- printFieldCli name newEnemyField newEnemyShips
                                                        turnBot (newEnemyField, newEnemyShips, name)
                                                  else
                                                      return (enemyField, enemyShips)
                                        else
                                            turnBot (enemyField, enemyShips, name)

