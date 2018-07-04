module Main where

import Graphics.Gloss
import System.Random
import Control.Monad
import Prim

width, height, offset :: Int
width  = 640
height = 480
offset = 10

window :: Display
window = InWindow
  "Maze"           -- window title
  (width, height)  -- window size
  (offset, offset) -- window position

background :: Color -- background color
background = black

wayColor :: Color
wayColor = white

mkStart :: Float -> Float -> Picture
mkStart x y = translate x y $ color green $ rectangleSolid 1 1

mkEnd :: Float -> Float -> Picture
mkEnd x y = translate x y $ color red $ rectangleSolid 1 1
  
mkWay :: Float -> Float -> Picture
mkWay x y = translate x y $ color wayColor $ rectangleSolid 1 1
  

-- | Guarda o estado do jogo.
data MazeState = Game
  { cells :: [(Int, Int)],
    cols  :: Int,
    rows  :: Int }
  
-- | Renderiza o estado do jogo (converte pra Picture).
render :: MazeState -> Picture
render game = pictures $
  [
    mkWay (fromIntegral x) (fromIntegral y) | (x,y) <- cells game
  ] ++ [mkStart 0 0, mkEnd (2 * ((fromIntegral $ cols game) - 1)) (2 * ((fromIntegral $ rows game) - 1))]

-- | Converte de vertices e arestas para malha a ser pintada
graphToMaze :: Grafo -> [(Int, Int)]
graphToMaze g = [v | (v,_) <- g] ++
  [intermediarios | ((i,j), la) <- g,
    intermediarios <- [((x+i) `div` 2, (y+j) `div` 2) | (x,y) <- la]] ++
  [v | (_,la) <- g, v <- la]

-- | Inicializa o jogo com um estado.
initialState :: (RandomGen t) => t -> MazeState
initialState gen = Game
  { cells = graphToMaze $ gordifica $ generateMaze c r gen,
    cols = c,
    rows = r
  } where c = 42; r = 30
  
-- | Multiplica grafo por 2 para ficar bonitinho
gordifica :: Grafo -> Grafo
gordifica g = [((2*i,2*j),[(2*u, 2*v) | (u,v) <- la]) | ((i,j),la) <- g]

main = do
  ops <- getStdGen
  display window background (render $ initialState ops)
