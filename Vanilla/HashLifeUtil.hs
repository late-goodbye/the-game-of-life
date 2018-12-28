module HashLifeUtil where

  import qualified Data.HashTable.IO as HT
  import           Data.Hashable
  import           System.Mem.StableName
  import           System.Mem.Weak
  import Data.List
  import Data.List.Split (splitOneOf)
  import Data.Function

  type Quad a = (a,a,a,a)
  data Square = Square (Quad Square) | Dead | Alive deriving (Eq)

  instance (Hashable Square) where
    hashWithSalt salt Dead = salt
    hashWithSalt salt Alive = salt + 1
    hashWithSalt salt (Square q) = salt + hashWithSalt salt q

  instance (Show Square) where
    show Dead = "."
    show Alive = "*"
    show (Square (nw,ne,sw,se)) = concat $ addBreaks (concat' nw ne) ++ ["\n"] ++ addBreaks (concat' sw se)
      where
      concat' :: Square -> Square -> [String]
      concat' = zipWith (++) `on` ((splitOneOf "\n") . show)
      addBreaks = intersperse "\n"

  infixl 9 #

  data SubSquare = NW | NE | SW | SE

  (#) :: Square -> SubSquare -> Square
  Square (nw,ne,sw,se) # NW = nw
  Square (nw,ne,sw,se) # NE = ne
  Square (nw,ne,sw,se) # SW = sw
  Square (nw,ne,sw,se) # SE = se

  type SquareCons = Quad Square -> IO Square
  type SquareCalc = Square -> IO Square

  type MemoTable name val = HT.BasicHashTable name (Weak val)

--  newtype QSN a = Quad (StableName a)
--
--  instance (Hashable a) => (Hashable (QSN a)) where
--    hashWithSalt salt (nw,ne,sw,se) = salt + nwh*3 + neh*5 + swh*7 + seh*9
--      where
--        nwh = hashWithSalt salt nw
--        neh = hashWithSalt salt ne
--        swh = hashWithSalt salt sw
--        seh = hashWithSalt salt se

  makeStableName4 :: Quad a -> IO (Quad (StableName a))
  makeStableName4 (nw,ne,sw,se) = do
      nwn <- makeStableName nw
      nen <- makeStableName ne
      swn <- makeStableName sw
      sen <- makeStableName se
      return (nwn,nen,swn,sen)

  memoize :: (Eq name, Hashable name) => MemoTable name val -> (key -> IO name) -> (key -> IO val)
          -> key -> IO val
  memoize tbl mkname mkval key = do
      sn <- mkname key
      found <- HT.lookup tbl sn
      case found of
        Nothing -> cons sn
        Just ptr -> do
          maybe <- deRefWeak ptr
          case maybe of
            Nothing -> cons sn
            Just val -> return val
    where
      cons sn = do
        let gc = HT.delete tbl sn
        val <- mkval key
        wp <- mkWeak key val (Just gc)
        HT.insert tbl sn wp
        return val


  memoins :: (Eq name, Hashable name) => MemoTable name val -> (key -> IO name) -> key -> val -> IO ()
  memoins tbl mkname key val = do
      sn <- mkname key
      wp <- mkWeak key val Nothing
      HT.insert tbl sn wp

  memoize' tbl = memoize tbl makeStableName4
  memoins' tbl = memoins tbl makeStableName4

  sq_0, sq_1, sq_2 :: [Square]
  sq_0 = [ Dead, Alive ]
  sq_1 = [ Square (nw,ne,sw,se)
         | nw <- sq_0, ne <- sq_0, sw <- sq_0, se <- sq_0 ]
  sq_2 = [ Square (nw,ne,sw,se)
         | nw <- sq_1, ne <- sq_1, sw <- sq_1, se <- sq_1 ]

  pop Dead = 0
  pop Alive = 1

  calc_3x3 ::  Square -> Square -> Square
            -> Square -> Square -> Square
            -> Square -> Square -> Square -> Square
  calc_3x3 nw nn ne
           ww cc ee
           sw ss se =  if count == 2 then  cc   else
                       if count == 3 then Alive else Dead
           where
           count = pop nw + pop nn + pop ne
                 + pop ww          + pop ee
                 + pop sw + pop ss + pop se

  calc_4x4 :: SquareCons -> Quad Square -> IO Square
  calc_4x4 cons (nw,ne,sw,se) = let
      nwr = calc_3x3 (nw#NW) (nw#NE) (ne#NW)
                     (nw#SW) (nw#SE) (ne#SW)
                     (sw#NW) (sw#NE) (se#NW)
      ner = calc_3x3 (nw#NE) (ne#NW) (ne#NE)
                     (nw#SE) (ne#SW) (ne#SE)
                     (sw#NE) (se#NW) (se#NE)
      swr = calc_3x3 (nw#SW) (nw#SE) (ne#SW)
                     (sw#NW) (sw#NE) (se#NW)
                     (sw#SW) (sw#SE) (se#SW)
      ser = calc_3x3 (nw#SE) (ne#SW) (ne#SE)
                     (sw#NE) (se#NW) (se#NE)
                     (sw#SE) (se#SW) (se#SE)
      in
      cons (nwr,ner,swr,ser)

  get_nw cons (nw,ne,sw,se) = return nw
  get_nn cons (nw,ne,sw,se) = cons (nw#NE, ne#NW, nw#SE, ne#SW)
  get_ne cons (nw,ne,sw,se) = return ne
  get_ww cons (nw,ne,sw,se) = cons (nw#SW, nw#SE, sw#NW, sw#NE)
  get_cc cons (nw,ne,sw,se) = cons (nw#SE, ne#SW, sw#NE, se#NW)
  get_ee cons (nw,ne,sw,se) = cons (ne#SW, ne#SE, se#NW, se#NE)
  get_sw cons (nw,ne,sw,se) = return sw
  get_ss cons (nw,ne,sw,se) = cons (sw#NE, se#NW, sw#SE, se#SW)
  get_se cons (nw,ne,sw,se) = return se

  calc' :: SquareCons -> SquareCalc -> Quad Square -> IO Square
  calc' cons rec q = do
      putStrLn $ "calc' on " ++ show q
      nw1 <- rec =<< get_nw cons q
      nn1 <- rec =<< get_nn cons q
      ne1 <- rec =<< get_ne cons q
      ww1 <- rec =<< get_ww cons q
      cc1 <- rec =<< get_cc cons q
      ee1 <- rec =<< get_ee cons q
      sw1 <- rec =<< get_sw cons q
      ss1 <- rec =<< get_ss cons q
      se1 <- rec =<< get_se cons q
      
      nw2 <- rec =<< cons (nw1, nn1, ww1, cc1)
      ne2 <- rec =<< cons (nn1, ne1, cc1, ee1)
      sw2 <- rec =<< cons (ww1, cc1, sw1, ss1)
      se2 <- rec =<< cons (cc1, ee1, ss1, se1)
      cons (nw2, ne2, sw2, se2)

  init_cons :: IO SquareCons
  init_cons = do
      tbl <- HT.new
      let add (Square q) = memoins' tbl q q
      mapM_ add (sq_1 ++ sq_2)

      let cons k = (memoize' tbl return k) >>= (return . Square)
      return cons

  init_calc :: SquareCons -> IO SquareCalc
  init_calc cons = do
      tbl <- HT.new
      let add (Square q) = do {val <- calc_4x4 cons q; memoins' tbl q val}
      
      mapM_ add sq_2

      let calc s | (Square q) <- s = memoize' tbl (calc' cons calc) q
                 | otherwise = return s
      return calc
