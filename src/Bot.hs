module Bot where
import System.Random
import Types

generateShip :: [Coordinate] -> Int -> Int -> IO Ship
generateShip [] len direction = do
                          f <- newStdGen
                          g <- newStdGen
                          let coord = (fst(randomR (0, 9) f) :: Int, fst(randomR (0, 9) g) :: Int)
                          generateShip ([coord]) (len-1) direction
generateShip list 0 direction = return list
generateShip list len direction | direction == 0 = generateShip (list ++ [(fst(last list), snd(last list) + 1)]) (len - 1) direction
                                | direction == 1 = generateShip (list ++ [(fst(last list) + 1, snd(last list))]) (len - 1) direction
                                
                                
generateCoordinate :: Field -> IO Coordinate
generateCoordinate field = do
                            f <- newStdGen
                            g <- newStdGen
                            let x = fst(randomR (0, 9) f) :: Int
                            let y = fst(randomR (0, 9) g) :: Int
                            if last (take y (last(take x field))) == False then
                              do
                                let coord = (x, y)
                                return coord
                            else
                                generateCoordinate field