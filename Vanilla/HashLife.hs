module HashLife where

  import HashLifeUtil
  import Data.List.Split
  import Data.Function

  instance (Show Square) where
    show Dead = "."
    show Alive = "*"
    show (Square (nw,ne,sw,se)) = concat $ (concat' nw ne) ++ ["\n"] ++ (concat' sw se)
      where
      concat' :: Square -> Square -> [String]
      concat' = zipWith (++) `on` ((splitOneOf "\n") . show)


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