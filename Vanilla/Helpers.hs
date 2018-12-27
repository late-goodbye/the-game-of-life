module Helpers where

  import System.Console.ANSI
  import System.IO
  import Data.List.Split (chunksOf)

  import Control.Concurrent

  printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 5 arr

  resetScreen :: IO ()
  resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

  pause :: Int -> IO ()
  pause ms = hFlush stdout >> threadDelay (ms*1000)

