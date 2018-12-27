module Helpers where

  import System.Console.ANSI
  import System.IO

  import Control.Concurrent

  splitEvery _ [] = []
  splitEvery n list = first : (splitEvery n rest)
    where
      (first,rest) = splitAt n list

  printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ splitEvery 5 arr

  resetScreen :: IO ()
  resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

  pause :: Int -> IO ()
  pause ms = hFlush stdout >> threadDelay (ms*1000)

