module HashLife where

  import HashLifeUtil
  import Data.List

  fromMatrix :: [[Bool]] -> Square
  fromMatrix [[True]] = Alive
  fromMatrix [[False]] = Dead
  fromMatrix m = Square (fromMatrix nw, fromMatrix ne, fromMatrix sw, fromMatrix se)
    where
    splitHalf l = splitAt ((length l) `div` 2) l
    mHalf = splitHalf m
    (n, s) = (map splitHalf (fst mHalf), map splitHalf (snd mHalf))
    nw = map fst n
    ne = map snd n
    sw = map fst s
    se = map snd s

  fromStrings ss = fromMatrix $ map (map toBool) ss
    where toBool c = c == '*'

  oneStep s = do
    cons <- init_cons
    calc <- init_calc cons
    calc s