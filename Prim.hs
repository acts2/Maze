module Prim where

type Vertice = (Int, Int)
type ListaAdj = (Vertice, [Vertice])

type Grafo = [ListaAdj]

inicializaGrafoConexo :: Int -> Int -> Grafo
inicializaGrafoConexo width height = listaAdj
  where
    vertices  = [(i, j) | i <- [0..width-1], j <- [0..height-1]]
    listaAdj  = [((i,j), verticesAdjacentes (i,j) width height) | (i, j) <- vertices]

verticesAdjacentes vertice width height = map (\((i,x),(j,y)) -> (i+x, j+y)) $ filter (\((i,x),(j,y)) -> if (x+i >= height || y+j >= width || x+i < 0 || y+j < 0 || (x==y) || x+y == 0) then False else True) [((i,x), (j,y)) | let (i, j) = vertice, x <- [(-1), 0, 1], y <- [(-1), 0, 1]]
