module Game where

  import Types
  import Text.Printf
  import Data.List (transpose)
  import Data.Tuple (swap)
  import Control.Monad (replicateM_)
  import Universe
  import Cell (isAlive)
  import Helpers (printArray)
  import RandomStuff

  getNeighbours :: Universe -> Cell -> [Cell]
  getNeighbours u c = (filter $ isNeighbourOf c) u
    where
      isNeighbourOf :: Cell -> Cell -> Bool
      cell1 `isNeighbourOf` cell2 = (x1 /= x2 || y1 /= y2) && (x1 `closeAtX` x2) && (y1 `closeAtY` y2)
        where
          y1 = fst . fst $ cell1
          x1 = snd . fst $ cell1

          y2 = fst . fst $ cell2
          x2 = snd . fst $ cell2

          closeAt :: Int -> Int -> Int -> Bool
          closeAt c a b = (abs (a - b) < 2) || (abs (a - b) >= c)

          closeAtX :: Int -> Int -> Bool
          closeAtX = closeAt widthOfUniverse

          closeAtY :: Int -> Int -> Bool
          closeAtY = closeAt heightOfUniverse

          lc = last u

          widthOfUniverse = snd (fst lc)
          heightOfUniverse = fst (fst lc)

  getAliveCells :: Universe -> [Cell]
  getAliveCells u = (filter isAlive) u

  getAliveNeighbours :: Universe -> Cell -> [Cell]
  getAliveNeighbours u c = (filter isAlive) (getNeighbours u c)

  getSimulatedData :: Int -> Int -> Int -> [[Bool]]
  getSimulatedData h w steps = map (map isAlive) (u h w)
    where
      u h w = simulateUniverse steps $ populateUniverseWithGlider $ createUniverse h w

  predictFuture :: Universe -> Universe
  predictFuture u = (map predict) u
    where
      predict cell
        | present && ((length neighbours) > 3 || (length neighbours) < 2) = ((x, y), (present, False))
        | (not present) && ((length neighbours) == 3) = ((x, y), (present, True))
        | otherwise = ((x, y), (present, present))
        where
          neighbours = getAliveNeighbours u cell

          x = fst . fst $ cell
          y = snd . fst $ cell

          present = fst . snd $ cell
          future = snd . snd $ cell

  simulateUniverse :: Int -> Universe -> Multiverse
  simulateUniverse 0 universe = [universe]
  simulateUniverse epoch universe = universe : simulateUniverse (epoch - 1) (transformUniverse universe)

  transformUniverse :: Universe -> Universe
  transformUniverse u = actualizeUniverse $ predictFuture u


  run h w steps = printMultiverse $ simulateUniverse steps $ populateUniverseWithGlider $ createUniverse h w
  runDebug = printArray $ getNeighbours (populateUniverseWithGlider $ createUniverse 5 10) ((1, 0),(True, False))
