module RandomStuff where
  import System.Random
  import System.IO

  randomBool :: Double -> IO Bool
  randomBool prob = do
    rnd <- randomIO
    return $ rnd < prob
