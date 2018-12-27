module Main where

  import Game (run, runDebug, runRandom)
  import System.IO

  prompt s = do
    putStr s
    hFlush stdout
    line <- getLine
    return (read line)

  main = do
    h <- prompt "Map height: "
    w <- prompt "Max width: "
    steps <- prompt "How many steps to simulate: "
    runRandom h w steps 3
