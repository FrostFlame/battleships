module Logic where

import Types
import Util
import WorkWithCLI
import Bot
import Control.Monad

markShot :: Field -> Int -> Int -> CellState -> Field
markShot field x y state  | select y (select x field) == Dead = field
                          | otherwise = replace x field (replace y (field !! x) state)

                                        

markMissList :: Field -> [Coordinate] -> Field
markMissList field [] = field
markMissList field (x:xs) | validateCoordinate x = markMissList (markShot field (snd x) (fst x) Miss ) xs
                          | otherwise = markMissList field xs


markDeadShip :: Field -> Ship -> Field
markDeadShip field [] = field
markDeadShip field (x:xs) = markDeadShip (markShot (markMissList field (getAround x)) (snd x) (fst x) Dead  ) xs


mark :: Field -> Coordinate -> CellState -> [Ship] -> Field
mark field (-1, -1) _ [] = field
mark field (-1, -1) _ (x:xs) = mark (markDeadShip field (checkShipDestroyed field x)) (-1, -1) Dead xs
mark field coord state ships | select (fst coord) (select (snd coord) field) == Dead = field
                             | otherwise = mark (markShot field (snd coord) (fst coord) state) (-1, -1) Dead ships


removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs


checkShipNotDestroyed :: Field -> Ship -> Ship
checkShipNotDestroyed field ship = if and [select (fst coord) (select (snd coord) field) == Dead | coord <- ship] == False then
                                                   ship
                                               else
                                                   []


checkShipDestroyed :: Field -> Ship -> Ship
checkShipDestroyed field ship = if and [select (fst coord) (select (snd coord) field) == Hit | coord <- ship] == True then
                                                   ship
                                               else
                                                   []


fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate =  let flag = if or [coordinate == coord | ship <- enemyShips, coord <- ship] then True else False
                                                state = if flag then Hit else Miss
                                                newEnemyField = mark enemyField coordinate state enemyShips
                                            in (newEnemyField,
                                            removeDestroyedShips [checkShipNotDestroyed newEnemyField ship | ship <- enemyShips], flag)


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
                                                  return (newEnemyField, newEnemyShips)
                                              else
                                                do
                                                  if hit then
                                                      do
                                                        printFieldCli name newEnemyField newEnemyShips
                                                        turn (newEnemyField, newEnemyShips, name)
                                                  else
                                                      return (newEnemyField, newEnemyShips)
                                        else
                                            turn (enemyField, enemyShips, name)
                                            
                                            
                                            
turnBot :: (Field, [Ship], String) -> IO (Field, [Ship])
turnBot (enemyField, enemyShips, name) = do
                                        coord <- checkHitShip enemyField (0,0)
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
                                                      return (newEnemyField, newEnemyShips)
                                        else
                                            turnBot (enemyField, enemyShips, name)
