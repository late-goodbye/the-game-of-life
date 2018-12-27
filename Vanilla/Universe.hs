module Universe where

  import Types
  import Cell
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
  populateUniverse :: Universe -> Universe
  populateUniverse u = (map revive) u
    where
      revive cell@((x,y),_)
          -- x has to be equal to a random value here, not zero
        | x == 0 = ((y, x), True)
        | otherwise = cell

  populateUniverseWithGlider :: Universe -> Universe
  populateUniverseWithGlider u = (map revive) u
    where
      revive cell@((x,y),_)
        | (x, y) `elem` glider = ((x, y), True)
        | otherwise = cell
        where
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
