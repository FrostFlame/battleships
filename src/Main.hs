module Main where

import Types
import System.Random
import Bot
import Util
import WorkWithCLI
import Logic


initField :: Field
initField = take fieldSize (repeat (take fieldSize (repeat Empty)))


inputShip :: [Ship] -> Int -> String -> IO Ship
inputShip placedShips len player = do
                                    coords <- if (player == botname)
                                      then do
                                        r <- randomRIO(0, 1)
                                        generateShip [] len r
                                      else getShipFromCli len
                                    
                                    if validateShipCoordinates placedShips coords len then
                                       return coords
                                    else
                                       inputShip placedShips len player


inputShips :: Int -> [Ship] -> String -> IO [Ship]
inputShips listSize placedShips player =  if listSize < shipCount then
                                            do
                                              ship <- inputShip placedShips (shipLengthList !! listSize) player
                                              shipList <- (inputShips (listSize + 1) (ship : placedShips) player)
                                              return (ship : shipList)
                                          else
                                            return []


inputShipsStrat :: IO [Ship]
inputShipsStrat = do
                                                shipList <- do
                                                      side <- randomRIO(0, 3)
                                                      line <- randomRIO(0, 1)
                                                      generateByStrategy [] side line
                                                return shipList
         

game :: [String] -> [Field] -> [[Ship]] -> IO ()
game names fields ships = do
                                      putStrLn ("\n" ++ head names ++ "'s turn")
                                      (newField, newShipList) <- if head names /= botname 
                                        then do
                                            printMyFieldCli (head names) (head fields) (head ships)
                                            printFieldCli (last names) (last fields) (last ships)
                                            turn (last fields, last ships, last names)
                                        else do
                                            turnBot (last fields, last ships, last names)

                                      if length newShipList == 0 then
                                          do
                                            putStrLn ("\n" ++ head names ++ " won!\n")
                                            printFieldCli (last names) newField newShipList
                                            printFieldCli (head names) (head fields) (head ships)
                                      else
                                          game [last names, head names] [newField, head fields] [newShipList, head ships]


main :: IO ()
main = do
         putStrLn "What is the name of the player?"
         name <- getLine
         let names = [name, botname]
         putStrLn (name ++ ", enter your ships")
         shipsPlayer <- inputShips 0 [] name
         --to do add computer ships
         strat <- randomRIO(0, 1) :: IO Int
         shipsComputer <- if (strat == 0)
          then
            inputShips 0 [] botname
          else
            inputShipsStrat

         printMyFieldCli "bot" initField shipsComputer


         game names [initField, initField] [shipsPlayer, shipsComputer]