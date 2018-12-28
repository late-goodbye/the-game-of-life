{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards    #-}


module Printing where
import Graphics.Gloss
import System.Random
import Control.Monad
import Types
import Cell
import Graphics.Gloss.Interface.Pure.Simulate
import qualified Data.Vector    as Vec
-- | Тут формат клетки для вывода
data CellForPrinting
        = -- |Живая
          CellAlive 

          -- | Мертвая клетка
        | CellDead
        deriving (Show, Eq)


-- | Sort the living from the dead.
isAliveCellOld :: CellForPrinting -> Bool
isAliveCellOld cellold
 = case cellold of
        CellAlive      -> True
        CellDead        -> False


-- | The basic shape of a cellold.
cellShape :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi
 = let  cs      = fromIntegral cellSize
        posX    = fromIntegral posXi
        posY    = fromIntegral posYi
        x1      = posX
        x2      = posX + cs
        y1      = posY
        y2      = posY + cs
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

redCol ::  Color 
redCol  =      red

greenCol ::  Color 
greenCol  =      green
-- | Получаю картинку клетки с нужным цветом
pictureOfCell ::  Int -> Int -> Int -> CellForPrinting -> Picture
pictureOfCell  cellSize posX posY cellold
 = case cellold of
        CellAlive    -> Color greenCol  (cellShape cellSize posX posY)
        CellDead        -> Color redCol            (cellShape cellSize posX posY)
		

        

		
		
		

getrealWorld::Universe -> World
getrealWorld u = getTheWorld( getTheWorlds u)
		
printing ::  Universe -> IO ()
printing universe 
 = do
		
        let another = getrealWorld universe

        
	
        simulate (InWindow "Haskell project team DUCK"
                           (1500,800) (5, 5))
                white 10 another drawWorld simulateWorld
		



getTheBools :: Universe -> [Bool]
getTheBools u = do
    cell <- u
    [mark cell] 
    where
      mark cell
        | isAlive cell =  True
        | otherwise =  False

getTheWorlds :: Universe ->   [World]
getTheWorlds u
 = do   bools   <- replicateM (10 * 10) getTheBools u
        return  $ World
                { worldCells            = Vec.fromList $map cellOfBool bools
                , worldWidth            = 10
                , worldHeight           = 10
                , worldCellSize         = 40
                , worldCellSpace        =5
                , worldSimulationPeriod = 0.9
                , worldElapsedTime      = 0 }

getTheWorld :: [World] ->   World
getTheWorld w =  head w






drawWorld
        :: World
        -> Picture

drawWorld world
 = let  (windowWidth, windowHeight)
                = windowSizeOfWorld world

        offsetX = - fromIntegral windowWidth  / 2
        offsetY = - fromIntegral windowHeight / 2
   in   Translate offsetX offsetY
                $ Pictures
                $ Vec.toList
                $ Vec.imap (drawCell world) (worldCells world)


-- | Получаю картинку клетки в нужной координате
drawCell :: World -> Index -> CellForPrinting -> Picture
drawCell world index cellold
 = let  cs      = fromIntegral (worldCellSize world)
        cp      = fromIntegral (worldCellSpace world)

        (x, y)  = coordOfIndex world index
        fx      = fromIntegral x * (cs + cp) + 1
        fy      = fromIntegral y * (cs + cp) + 1

   in   pictureOfCell
                
                (worldCellSize   world)
                fx
                fy
                cellold


-- |Тут по идее получаем размер окна( на практике не работает)
windowSizeOfWorld :: World -> (Int, Int)
windowSizeOfWorld world
 = let  cellSize        = worldCellSize world
        cellSpace       = worldCellSpace world
        cellPad         = cellSize + cellSpace
        height          = cellPad * (worldHeight world) + cellSpace
        width           = cellPad * (worldWidth  world) + cellSpace
   in   (width, height)
   




type Vec        = Vec.Vector

-- Тут хранятся индекса клеток
type Index      = Int

-- | тут хранятся координаты

type Coord      = (Int, Int)

indexOfCoord :: World -> Coord -> Index
indexOfCoord world (x, y)
        = x + y * (worldWidth world)

coordOfIndex :: World -> Index -> Coord
coordOfIndex world i
        = ( i `mod` worldWidth world
          , i `div` worldWidth world)


-- World ----------------------------------------------------------------------
data World
        = World
        { worldCells            :: Vec CellForPrinting
        , worldWidth            :: Int
        , worldHeight           :: Int

        -- | Размер клетки
        , worldCellSize         :: Int

        -- | размер пространства между клетками
        , worldCellSpace        :: Int      

        -- | как долго выводим одно состояние
        , worldSimulationPeriod :: Float

        -- | Нужно, чтобы проверять на состояние
        , worldElapsedTime      :: Float }




				


-- | Из булевого значения получаем клетку
cellOfBool :: Bool -> CellForPrinting
cellOfBool b
 = case b of
        True    -> CellAlive 
        False   -> CellDead


-- |Получаем клетку в нужном состоянии
getCell :: World -> Coord -> CellForPrinting
getCell world coord@(x, y)
        | x < 0 || x >= worldWidth  world       = CellDead
        | y < 0 || y >= worldHeight world       = CellDead

        | otherwise
        = worldCells world Vec.! indexOfCoord world coord


-- | получаем соседей клетки
getNeighbourhood :: World -> Coord -> [CellForPrinting]
getNeighbourhood world (ix, iy)
 = let  indexes = [ (x, y)
                        | x <- [ix - 1 .. ix + 1]
                        , y <- [iy - 1 .. iy + 1]
                        , not (x == ix && y == iy) ]
   in   map (getCell world) indexes


-- | и вычисляем ее следующее состояние
stepCell :: CellForPrinting -> [CellForPrinting] -> CellForPrinting
stepCell cellForPrinting neighbours
 = let  live    = length (filter isAliveCellOld neighbours)
   in   case cellForPrinting of
         CellAlive   -> if elem live [2, 3] then CellAlive  else CellDead
         CellDead       -> if live == 3        then CellAlive          else CellDead


-- | получаем следующее состояние клетки
stepIndex :: World -> Int -> CellForPrinting -> CellForPrinting
stepIndex world index cellForPrinting
 = let  coord   = coordOfIndex world index
        neigh   = getNeighbourhood world coord
   in   stepCell cellForPrinting neigh


-- | полуачем следующее состояние мира
stepWorld :: World -> World
stepWorld world
        = world { worldCells = Vec.imap (stepIndex world) (worldCells world) }


-- | Реализуем функцию симуляции
simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld _ time world

        -- Тут проверка на то, прошло ли достаточно времени
        | worldElapsedTime world >= (worldSimulationPeriod world)
        = let world'    = stepWorld world
          in  world' { worldElapsedTime = 0 }

        -- Если не прошло, то ждем еще.
        | otherwise
        = world { worldElapsedTime = worldElapsedTime world + time }