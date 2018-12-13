module Bot where
import System.Random
import Types
import Util

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
                            
                            if last (take (y + 1) (last(take (x + 1) field))) == Empty then
                              do
                                let coord = (x, y)
                                return coord
                            else
                                generateCoordinate field

generateNearby :: Coordinate -> Field -> IO Coordinate
generateNearby coord field = do
                        f <- newStdGen
                        let dir = fst(randomR (0, 3) f) :: Int
                        --Добавить все варианты в список и проверить, что есть не пустая. Иначе вызываю checkHitShip на следующей (y+1). Проверить, не находится ли координата у края.
                        let newCoord1 = (fst coord, snd coord + 1)
                        let newCoord2 = (fst coord, snd coord - 1)
                        let newCoord3 = (fst coord + 1, snd coord)
                        let newCoord4 = (fst coord - 1, snd coord)
                        if last (take ((snd newCoord1) + 1) (last(take (fst(newCoord1) + 1) field))) == Empty then
                              do
                                return newCoord1
                        else
                            do
                            if last (take ((snd newCoord2) + 1) (last(take (fst(newCoord2) + 1) field))) == Empty then
                                  do
                                    return newCoord2
                            else
                                  do
                                  if last (take ((snd newCoord3) + 1) (last(take (fst(newCoord3) + 1) field))) == Empty then
                                        do
                                          return newCoord3
                                  else
                                        do
                                        if last (take ((snd newCoord4) + 1) (last(take (fst(newCoord4) + 1) field))) == Empty then
                                              do
                                                return newCoord4
                                        else
                                              do
                                                let newCoord | (snd coord == 9) = (fst coord + 1, 0)
                                                             | otherwise = (fst coord, snd coord + 1)
                                                (checkHitShip field newCoord)

checkHitShip :: Field -> Coordinate -> IO Coordinate
checkHitShip enemyField coord
                              | and [(select (fst coord) (select (snd coord) enemyField) /= Hit), (fst coord == 9), (snd coord == 9)] = generateCoordinate enemyField
                              | and [(select (fst coord) (select (snd coord) enemyField) /= Hit), (snd coord < 9)] = do
                                                                                                                        checkHitShip enemyField (fst coord, (snd coord) + 1)
                              | and [(select (fst coord) (select (snd coord) enemyField) /= Hit), (snd coord == 9)] = do
                                                                                                                        checkHitShip enemyField ((fst coord) + 1, 0)
                              | otherwise = do
                                                print "otherwise"
                                                generateNearby coord enemyField