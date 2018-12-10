module WorkWithCLI where

import Types
import Util


getShipFromCli :: Int -> IO Ship
getShipFromCli len = do
                    putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                    string <- getLine
                    let stringCoords = splitCoordinatesInString string
                    let coords = map convertStringToCoordinates stringCoords
                    return coords
                    
printFieldCli :: String -> Field -> [Ship] -> IO ()
printFieldCli playerName field ships = do
                                      putStrLn (playerName ++ "'s field:")
                                      putStrLn ("#0123456789#\n0" ++ convertFieldToString field ships (0, 0) 0 ++ "#0123456789#")
                                      putStrLn ""


printMyFieldCli :: String -> Field -> [Ship] -> IO ()
printMyFieldCli playerName field ships = do
                                        putStrLn (playerName ++ "'s field:")
                                        putStrLn ("#0123456789#\n0" ++ convertMyFieldToString field ships (0, 0) 0 ++ "#0123456789#")
                                        putStrLn ""
                                      


getCoordinateCli :: IO String                                
getCoordinateCli = do
                    putStrLn ("Enter the coordinates to fire shot")
                    string <- getLine
                    return string
                    
printHitCli :: Coordinate -> IO ()               
printHitCli coord = putStrLn ("Firing at coordinate (" ++ show ((fst coord)) ++ "," ++ show ((snd coord)) ++ "), Hit")


printMissCli :: Coordinate -> IO ()               
printMissCli coord = putStrLn ("Firing at coordinate (" ++ show ((fst coord)) ++ "," ++ show ((snd coord)) ++ "), Miss")

printSunkCli ::  IO ()               
printSunkCli = putStrLn "You sunk my battleship!"