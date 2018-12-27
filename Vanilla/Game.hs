module Game where

  import Types
  import Universe
  import Cell (isAlive)
  import Helpers (printArray)
  import RandomStuff

  getNeighbours :: Universe -> Cell -> [Cell]
  getNeighbours u c = (filter $ isNeighbourOf c) u
    where
      isNeighbourOf :: Cell -> Cell -> Bool
      ((x1,y1),_) `isNeighbourOf` ((x2,y2),_) = (x1 /= x2 || y1 /= y2) && (x1 `closeAtX` x2) && (y1 `closeAtY` y2)
        where
          closeAt :: Int -> Int -> Int -> Bool
          closeAt c a b = (abs (a - b) < 2) || (abs (a - b) >= c)

          closeAtX :: Int -> Int -> Bool
          closeAtX = closeAt widthOfUniverse

          closeAtY :: Int -> Int -> Bool
          closeAtY = closeAt heightOfUniverse

          ((widthOfUniverse, heightOfUniverse),_) = last u

  getAliveNeighbours :: Universe -> Cell -> [Cell]
  getAliveNeighbours u c = (filter isAlive) (getNeighbours u c)

  predictFuture :: Universe -> Universe
  predictFuture u = (map predict) u
    where
      predict cell@((x,y),alive)
        | nc == 2 = ((x, y), alive)
        | nc == 3 = ((x, y), True)
        | otherwise = ((x, y), False)
        where
          neighbours = getAliveNeighbours u cell
          nc = length neighbours

  simulateUniverse :: Universe -> Multiverse
  simulateUniverse universe = universe : simulateUniverse (predictFuture universe)

  run h w steps = printMultiverse $ (take steps) $ simulateUniverse $ populateUniverseWithGlider $ createUniverse h w
  runDebug = printArray $ getNeighbours (populateUniverseWithGlider $ createUniverse 5 10) ((1, 0),True)
  runRandom h w steps density = generateUniverse h w density >>= (printMultiverse . (take steps) . simulateUniverse)
