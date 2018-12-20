module Bot where
import System.Random
import Types
import Util
import System.Random.Shuffle

generateShip :: [Coordinate] -> Int -> Int -> IO Ship
generateShip [] len direction = do
                          f <- newStdGen
                          g <- newStdGen
                          let coord = (fst(randomR (0, 9) f) :: Int, fst(randomR (0, 9) g) :: Int)
                          generateShip ([coord]) (len-1) direction
generateShip list 0 direction = return list
generateShip list len direction | direction == 0 = generateShip (list ++ [(fst(last list), snd(last list) + 1)]) (len - 1) direction
                                | direction == 1 = generateShip (list ++ [(fst(last list) + 1, snd(last list))]) (len - 1) direction
 

getShip :: Ship -> Int -> Int -> Int -> Int -> Ship
getShip ship x y 0 dir = ship
getShip ship x y len dir | dir == 1 = getShip (ship ++ [(y, x)]) (x + 1) y (len - 1) dir
                         | dir == 0 = getShip (ship ++ [(y, x)]) x (y + 1) (len - 1) dir

generateByStrategy :: [Ship] -> Int -> Int -> IO[Ship]
generateByStrategy list side line = do
                                      f <- newStdGen
                                      g <- newStdGen
                                      let len1 = shuffle' [2, 2, 4] 3 f
                                      let len2 = shuffle' [2, 3, 3] 3 g
                                      case side of
                                        0 -> case line of
                                          0 -> do
                                            return (list ++ [getShip [] 0 0 (select 0 len1) 0, getShip [] 0 ((select 0 len1) + 1) (select 1 len1) 0, getShip [] 0 ((select 0 len1) + (select 1 len1) + 2) (select 2 len1) 0,
                                              getShip [] 2 0 (select 0 len2) 0, getShip [] 2 ((select 0 len2) + 1) (select 1 len2) 0, getShip [] 2 ((select 0 len2) + (select 1 len2) + 2) (select 2 len2) 0])
                                          1 -> do
                                            return (list ++ [getShip [] 2 0 (select 0 len1) 0, getShip [] 2 ((select 0 len1) + 1) (select 1 len1) 0, getShip [] 2 ((select 0 len1) + (select 1 len1) + 2) (select 2 len1) 0,
                                              getShip [] 0 0 (select 0 len2) 0, getShip [] 0 ((select 0 len2) + 1) (select 1 len2) 0, getShip [] 0 ((select 0 len2) + (select 1 len2) + 2) (select 2 len2) 0])
                                        1 -> case line of
                                          0 -> do
                                            return (list ++ [getShip [] 0 9 (select 0 len1) 1, getShip [] ((select 0 len1) + 1) 9 (select 1 len1) 1, getShip [] ((select 0 len1) + (select 1 len1) + 2) 9 (select 2 len1) 1,
                                              getShip [] 0 7 (select 0 len2) 1, getShip [] ((select 0 len2) + 1) 7 (select 1 len2) 1, getShip [] ((select 0 len2) + (select 1 len2) + 2) 7 (select 2 len2) 1])
                                          1 -> do
                                            return (list ++ [getShip [] 0 7 (select 0 len1) 1, getShip [] ((select 0 len1) + 1) 7 (select 1 len1) 1, getShip [] ((select 0 len1) + (select 1 len1) + 2) 7 (select 2 len1) 1,
                                              getShip [] 0 9 (select 0 len2) 1, getShip [] ((select 0 len2) + 1) 9 (select 1 len2) 1, getShip [] ((select 0 len2) + (select 1 len2) + 2) 9 (select 2 len2) 1])
                                        2 -> case line of
                                          0 -> do
                                            return (list ++ [getShip [] 9 0 (select 0 len1) 0, getShip [] 9 ((select 0 len1) + 1) (select 1 len1) 0, getShip [] 9 ((select 0 len1) + (select 1 len1) + 2) (select 2 len1) 0,
                                              getShip [] 7 0 (select 0 len2) 0, getShip [] 7 ((select 0 len2) + 1) (select 1 len2) 0, getShip [] 7 ((select 0 len2) + (select 1 len2) + 2) (select 2 len2) 0])
                                          1 -> do
                                            return (list ++ [getShip [] 7 0 (select 0 len1) 0, getShip [] 7 ((select 0 len1) + 1) (select 1 len1) 0, getShip [] 7 ((select 0 len1) + (select 1 len1) + 2) (select 2 len1) 0,
                                              getShip [] 9 0 (select 0 len2) 0, getShip [] 9 ((select 0 len2) + 1) (select 1 len2) 0, getShip [] 9 ((select 0 len2) + (select 1 len2) + 2) (select 2 len2) 0])
                                        3 -> case line of
                                          0 -> do
                                            return (list ++ [getShip [] 0 0 (select 0 len1) 1, getShip [] ((select 0 len1) + 1) 0 (select 1 len1) 1, getShip [] ((select 0 len1) + (select 1 len1) + 2) 0 (select 2 len1) 1,
                                              getShip [] 0 2 (select 0 len2) 1, getShip [] ((select 0 len2) + 1) 2 (select 1 len2) 1, getShip [] ((select 0 len2) + (select 1 len2) + 2) 2 (select 2 len2) 1])
                                          1 -> do
                                            return (list ++ [getShip [] 0 2 (select 0 len1) 1, getShip [] ((select 0 len1) + 1) 2 (select 1 len1) 1, getShip [] ((select 0 len1) + (select 1 len1) + 2) 2 (select 2 len1) 1,
                                              getShip [] 0 0 (select 0 len2) 1, getShip [] ((select 0 len2) + 1) 0 (select 1 len2) 1, getShip [] ((select 0 len2) + (select 1 len2) + 2) 0 (select 2 len2) 1])
                                
                                
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
                        let newCoord1 = (fst coord, snd coord + 1)
                        let newCoord2 = (fst coord, snd coord - 1)
                        let newCoord3 = (fst coord + 1, snd coord)
                        let newCoord4 = (fst coord - 1, snd coord)
                        if and [validateCoordinate newCoord1, select (fst newCoord1) (select (snd newCoord1) field) == Empty] 
                          then
                              do
                                return newCoord1
                          else
                            do
                            if and [validateCoordinate newCoord2, select (fst newCoord2) (select (snd newCoord2) field) == Empty]
                              then
                                  do
                                    return newCoord2
                              else
                                  do
                                  if and [validateCoordinate newCoord3, select (fst newCoord3) (select (snd newCoord3) field) == Empty]
                                    then
                                        do
                                          return newCoord3
                                    else
                                        do
                                        if and [validateCoordinate newCoord4, select (fst newCoord4) (select (snd newCoord4) field) == Empty]
                                          then
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
                              | and [(select (fst coord) (select (snd coord) enemyField) /= Hit), (snd coord < 9)] = checkHitShip enemyField (fst coord, (snd coord) + 1)
                              | and [(select (fst coord) (select (snd coord) enemyField) /= Hit), (snd coord == 9)] = checkHitShip enemyField ((fst coord) + 1, 0)
                              | otherwise = generateNearby coord enemyField