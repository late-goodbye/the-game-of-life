module Cell where

  import Types

  isAlive :: Cell -> Bool
  isAlive cell = fst (snd cell)
