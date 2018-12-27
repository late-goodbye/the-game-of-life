module Universe where

  import Types
  import System.Console.ANSI (clearScreen)
  import Control.Concurrent (threadDelay)
  import Cell (isAlive)
  import RandomStuff (getRandomCellState)
  import Helpers

  createUniverse :: Int -> Int -> Universe
  createUniverse height width = [((y, x), False) | x <- [0..(height-1)],
                                                   y <- [0..(width-1)] ]

  formatUniverse :: Universe -> String
  formatUniverse u = do
    cell <- u
    [mark cell] ++ eol cell
    where
      mark cell
        | isAlive cell = '*'
        | otherwise = '.'
      eol cell
        | (fst . fst $ cell) == (fst . fst $ lc) = ['\n']
        | otherwise = []
      lc = last u

  -- Not implemented properly yet
  populateUniverse :: Int -> Universe -> Universe
  populateUniverse density u = (map $ revive density) u
    where
      revive density cell
        | (x + y * x + getRandomCellState) `mod` 10 < density = ((y, x), True)
        | otherwise = cell
        where
          y = fst . fst $ cell
          x = snd . fst $ cell

  populateUniverseWithGlider :: Universe -> Universe
  populateUniverseWithGlider u = (map revive) u
    where
      revive cell@((x,y),_)
        | (x, y) `elem` glider = ((x, y), True)
        | otherwise = cell
        where
          x = fst . fst $ cell
          y = snd . fst $ cell
          glider = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

  printUniverse :: Universe -> IO ()
  printUniverse u = do
    resetScreen
    putStrLn $ formatUniverse u
    pause 500

  printMultiverse :: Multiverse -> IO ()
  printMultiverse us = do
    mapM_ printUniverse us
    putStrLn "Fin."
