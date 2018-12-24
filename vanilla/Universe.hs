module Universe where

  import Types
  import System.Console.ANSI (clearScreen)
  import Control.Concurrent (threadDelay)
  import System.Posix.Unistd (sleep)
  import Cell

  actualizeUniverse :: Universe -> Universe
  actualizeUniverse u = (map actualize) u
    where
      actualize (node, state) = (node, (snd state, False))

  createUniverse :: Int -> Int -> Universe
  createUniverse height width = [((y, x), (False, False)) | x <- [0..height],
                                                            y <- [0..width] ]

  formatUniverse :: Universe -> String
  formatUniverse u = do
    cell <- u

    [mark cell] ++ eol cell
    where
      mark cell
        | isAlive cell = '*'
        | otherwise = ' '
      eol cell
        | fst (fst cell) == 10 = ['\n']
        | otherwise = []

  -- Not implemented properly yet
  populateUniverse :: Universe -> Universe
  populateUniverse u = (map revive) u
    where
      revive cell
          -- x has to be equal to a random value here, not zero
        | x == 0 = ((y, x), (True, False))
        | otherwise = cell
        where
          x = fst (fst cell)
          y = snd (fst cell)

  populateUniverseWithGlider :: Universe -> Universe
  populateUniverseWithGlider u = (map revive) u
    where
      revive cell
        | (x, y) `elem` glider = ((x, y), (True, False))
        | otherwise = cell
        where
          x = fst (fst cell)
          y = snd (fst cell)
          glider = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

  printUniverse :: Universe -> IO ()
  printUniverse u = do
    sleep 1
    clearScreen
    putStrLn $ formatUniverse u

  printMultiverse :: Multiverse -> IO ()
  printMultiverse [] = putStrLn "Fin."
  printMultiverse (u : us) = do
    printUniverse u
    printMultiverse us
