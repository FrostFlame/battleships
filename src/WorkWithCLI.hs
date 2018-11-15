module WorkWithCLI where

import Types
import Util


getShipFromCli :: Int -> IO Ship
getShipFromCli len= do
                    putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                    string <- getLine
                    let stringCoords = splitCoordinatesInString string
                    let coords = map convertStringToCoordinates stringCoords
                    return coords
                    
printFieldCli :: String -> Field -> [Ship] -> IO ()
printFieldCli playerName field ships = do
                                      putStrLn (playerName ++ "'s field:")
                                      putStrLn ("#0123456789#\n0" ++ convertFieldToString field ships (1, 1) 0 ++ "#0123456789#")
                                      putStrLn ""
                                      
                                      


getCoordinateCli :: IO String                                
getCoordinateCli = do
                    putStrLn ("Enter the coordinates to fire shot")
                    string <- getLine
                    return string
                    
printHitCli :: Coordinate -> IO ()               
printHitCli coord = putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Hit")


printMissCli :: Coordinate -> IO ()               
printMissCli coord = putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Miss")

printSunkCli ::  IO ()               
printSunkCli = putStrLn "You sunk my battleship!"