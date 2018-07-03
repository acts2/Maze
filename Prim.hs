module Prim where

type Vertice = (Int, Int)
type ListaAdj = (Vertice, [Vertice])

type Grafo = [ListaAdj]

inicializaGrafoConexo :: Int -> Int -> Grafo
inicializaGrafoConexo width height = listaAdj
  where
    vertices = [(i, j) | i <- [0..width-1], j <- [0..height-1]]
    listaAdj = [((i,j),vertices) | (i, j) <- vertices]

