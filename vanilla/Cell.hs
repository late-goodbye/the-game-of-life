module Cell where

  import Types

  closeTo :: Int -> Int -> Bool
  a `closeTo` b = abs (a - b) < 2

  isAlive :: Cell -> Bool
  isAlive cell = fst (snd cell)

  isNeighbourOf :: Cell -> Cell -> Bool
  cell1 `isNeighbourOf` cell2 = (x1 /= x2 || y1 /= y2) && (x1 `closeTo` x2) && (y1 `closeTo` y2)
    where
      x1 = fst (fst cell1)
      y1 = snd (fst cell1)

      x2 = fst (fst cell2)
      y2 = snd (fst cell2)
