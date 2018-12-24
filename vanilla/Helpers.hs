module Helpers where

  splitEvery _ [] = []
  splitEvery n list = first : (splitEvery n rest)
    where
      (first,rest) = splitAt n list

  printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ splitEvery 5 arr
