module Life where

{-

Rules::

1.0. Cells (in this case, cubes) with only 1.0 or less neighbours die, as if by lonliness.
2. If 5 cells surround an empty cell it becomes a live cell, as if by reproduction.
3. If a cell has 8 or more neighbours, it dies from overcrowding.
4. Any live cell with 4 or 5 cells lives on to the next generation.

-}

import List as L
import Set as S
--import Math.Vector3 (..)

--a board is a list of live cells and the int representing # of live cells
type Board = B Int (List Cell)

type alias Cell = (Int,Int,Int)

{- | Board info: max position is 20, min position is -20 in x,y,z directions
-}
genNeighbors : Cell -> List Cell
genNeighbors (x,y,z) = 
    [((x+1),y, z), ((x+1), (y+1), z), ((x+1), y, (z+1)),
     ((x+1), (y+1), (z+1)), ((x+1), (y-1), z), ((x+1), y, (z-1)),
     ((x+1), (y-1), (z-1)), ((x+1), (y-1), (z+1)), ((x+1), (y+1), (z-1)),
     (x, (y+1), z), (x, y, (z+1)), (x, (y+1), (z+1)),
     (x, (y-1), z), (x, y, (z-1)), (x, (y-1), (z-1)),
     (x, (y-1), (z+1)), (x, (y+1), (z-1)), ((x-1), y, z),
     ((x-1), (y-1), z), ((x-1), y, (z-1)), ((x-1), (y+1), z),
     ((x-1), (y+1), (z-1)), ((x-1), (y+1), (z+1)), ((x-1), (y-1), z),
     ((x-1), (y-1), (z-1)), ((x-1), y, (z+1))]

neighbors : Cell -> List Cell
neighbors c = []



maybeIf : a -> Bool -> Maybe a
maybeIf a b = if b then Just a else Nothing

isAlive : Cell -> Board -> Bool
isAlive c (B i alive) = L.member c alive 

--countNeighbors (genNeighbors c) (B i cs)--     
countNeighbors : List Cell -> Board -> Int
countNeighbors neighs (B i cs) =  
  case neighs of
    []-> 0
    x::xs -> if | L.member x cs -> 1 + countNeighbors xs (B i cs)
                | otherwise -> 0 + countNeighbors xs (B i cs)
      
initialBoard : Board
initialBoard = B 0 []
      
updateGame : Board -> Board
updateGame (B i alive) =
  
  
find : a -> List a -> Int
find c xs =
  case xs of
    [] -> 0
    x::xs' -> if | c==x -> 1
                 | otherwise -> 1 + find c xs' 

delete : a -> List a ->  List a
delete c cs =
  let n = find c cs in  
    (L.take (n-1) cs) ++ (L.drop n cs)

evolve : Cell-> Board -> List Cell
evolve c (B i alive) = 
  let n = countNeighbors (genNeighbors c) (B i alive) 
      mem = L.member c alive in
    case mem of
      True -> if (n>8 || n<1) then (delete c alive) else alive
      False -> if n==5 then c::alive else alive
     

  
evolveCells : List Cell -> Board -> List Cell
evolveCells cs (B i alive) =
  case cs of
    [] -> []
    c::cs' -> (evolve c (B i alive)) ++ (evolveCells cs' (B i alive))
  

