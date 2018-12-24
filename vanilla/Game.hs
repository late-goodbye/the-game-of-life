module Game where

  import Types
  import Text.Printf
  import Data.List (transpose)
  import Data.Tuple (swap)
  import Control.Monad (replicateM_)
  import Universe
  import Cell (isAlive, isNeighbourOf)
  import Helpers (printArray)
  import RandomStuff

  getNeighbours :: Universe -> Cell -> [Cell]
  getNeighbours u c = (filter $ isNeighbourOf c) u

  getAliveCells :: Universe -> [Cell]
  getAliveCells u = (filter isAlive) u

  getAliveNeighbours :: Universe -> Cell -> [Cell]
  getAliveNeighbours u c = (filter isAlive) (getNeighbours u c)

  predictFuture :: Universe -> Universe
  predictFuture u = (map predict) u
    where
      predict cell
        | present && ((length neighbours) > 3 || (length neighbours) < 2) = ((x, y), (present, False))
        | (not present) && ((length neighbours) == 3) = ((x, y), (present, True))
        | otherwise = ((x, y), (present, present))
        where
          neighbours = getAliveNeighbours u cell

          x = fst (fst cell)
          y = snd (fst cell)

          present = fst (snd cell)
          future = snd (snd cell)

  simulateUniverse :: Int -> Universe -> Multiverse
  simulateUniverse 0 universe = [universe]
  simulateUniverse epoch universe = universe : simulateUniverse (epoch - 1) (transformUniverse universe)

  transformUniverse :: Universe -> Universe
  transformUniverse u = actualizeUniverse $ predictFuture u


  run = printMultiverse $ simulateUniverse 100 $ populateUniverseWithGlider $ createUniverse 5 10
  runDebug = printArray $ getNeighbours (populateUniverseWithGlider $ createUniverse 5 10) ((1, 0),(True, False))
