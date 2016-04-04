module Model where

import Set as S
import open Pair

--  Locations in our world are integer coordinates
type Loc = Pair Int

type Life = S.Set Loc

{-- Our game is described by
    a width `w`
    a height `h`
    and a set of living locations `life` --}
type Game = 
  { w : Int
  , h : Int
  , life : Life
  }

--  A barren land
emptyGame : Int -> Int -> Game
emptyGame w h =
  { w = w
  , h = h
  , life = S.empty
  }

maybeIf : a -> Bool -> Maybe a
maybeIf a b = if b then Just a else Nothing

--  Checks if a cell is alive
isAlive : Game -> Loc -> Bool
isAlive {life} loc = S.member loc life

--  Relative neighbouring locations
neighbourLocs : [Loc]
neighbourLocs =
  [ (-1, -1) , ( 0, -1) , ( 1, -1)
  , (-1,  0)            , ( 1,  0)
  , (-1,  1) , ( 0,  1) , ( 1,  1)
  ]
  
  --  Retrieves all the neighbour locations given a cell, within the bounds of the game
getNeighbours : Game -> Loc -> [Loc]
getNeighbours {w, h} loc = map (pWrap (w, h) . (=+=) loc) neighbourLocs

--  Count how many neighbouring cells contain life
countNeighbours : Game -> Loc -> Int
countNeighbours game loc =
  let locs = getNeighbours game loc
      activeLocs = map (isAlive game) locs
  in length <| filter id activeLocs

-- Progresses the life of a single cell
progressCell : Game -> Loc -> Maybe Loc
progressCell game loc =
  let ns = countNeighbours game loc in
  if | ns == 3          -> Just loc
     | ns < 2 || ns > 3 -> Nothing
     | otherwise        -> maybeIf loc (isAlive game loc)

--  Determine which cells should be progressed
cellsToProgress : Game -> [Loc]
cellsToProgress game = concatMap (getNeighbours game) <| S.toList game.life

--  Progresses a game one iteration
progressGame : Game -> Game
progressGame game =
  let cells = cellsToProgress game
      cells' = justs <| map (progressCell game) cells
  in { game | life <- S.fromList cells' }
