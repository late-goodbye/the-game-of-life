module RandomStuff where
  import System.Random
  import System.IO
  import System.IO.Unsafe

  getRand :: Bool -> Int
  getRand i = unsafePerformIO randomIO

  getRandomCellState :: Int

  getRandomCellState = mod (getRand True) 1000000
