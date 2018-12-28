module Main where

  import Game (run, runDebug, runRandom)
  import System.IO
  import Graphics.Gloss
  import Printing (printing)
  prompt s = do
    putStr s
    hFlush stdout
    line <- getLine
    return (read line)

  main = do
    h <- prompt "Map height: "
    w <- prompt "Map width: "
    steps <- prompt "How many steps to simulate: "
    runRandom h w steps 0.3
	
	
