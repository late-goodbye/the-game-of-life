module RandomStuff where
  import Data.Time.Clock.POSIX (getPOSIXTime)
  -- I have no idea how the next three functions have to work
  nanoTime :: IO Integer
  t mul = round . (mul *) <$> getPOSIXTime
  nanoTime = t 1000000000

  getRandomNumber :: IO Integer
  getRandomNumber = do
    num <- nanoTime
    return num

  checkRandomNumber :: Int -> Bool
  checkRandomNumber num = (num `mod` 10) > 6
