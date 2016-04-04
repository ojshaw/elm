module Octtree where

{-
  type alias Pos = { 
  x : Int, 
  y : Int, 
  z : Int 
}  
-}

type alias Interval = {
  low : Float,
  high : Float
}

type alias BoundingCube = {
  x : Interval,
  y : Interval,
  z : Interval
}

boundingCube : Float -> Float -> Float -> Float ->Float ->Float ->BoundingCube
boundingCube minx maxx miny maxy minz maxz =
  (BoundingCube (Interval minx maxx) (Interval miny maxy) (Interval minz maxz))
  
{-| Get the width along the x,y,z axes
-}
zWidth : BoundingCube -> Float
zWidth box  =
  box.z.high - box.z.low
  
yWidth : BoundingCube -> Float
yWidth box = 
  box.y.high - box.y.low
  
xWidth : BoundingCube -> Float
xWidth box = 
  box.x.high - box.x.low

{-| Get the half heights along x,y,z axes to facilitate subdividing
-}  
halfz : BoundingCube -> Float
halfz box =
  zWidth box / 2    
  
halfy : BoundingCube -> Float
halfy box =
  yWidth box / 2
  
halfx : BoundingCube -> Float
halfx box =
  xWidth box / 2 
  
center : BoundingCube -> {x : Float, y : Float, z : Float}
center box = {
  x = box.x.low + halfx box,
  y = box.y.low + halfy box,
  z = box.z.low + halfz box }  
  
{-| Creating the boundingCube of subdivisions of cube
    four tops, four bottoms - 8 subdivisions altogether
-}    

topNE : BoundingCube ->BoundingCube
topNE box = 
  let minx = box.x.low + halfx box
      miny = box.y.low + halfy box
      minz = box.z.low + halfz box
  in
    boundingCube minx box.x.high miny box.y.high minz box.z.high

topNW : BoundingCube ->BoundingCube
topNW box =
  let maxx = box.x.high - halfx box
      miny = box.y.low + halfy box
      minz = box.z.low + halfz box
  in
    boundingCube box.x.low maxx miny box.y.high minz box.z.high

topSE : BoundingCube ->BoundingCube
topSE box = 
  let minx = box.x.low + halfx box
      maxy = box.y.high - halfy box
      minz = box.z.low + halfz box
  in
    boundingCube minx box.x.high box.y.low maxy minz box.z.high

topSW : BoundingCube ->BoundingCube
topSW box =
  let maxx = box.x.high - halfx box
      maxy = box.y.high - halfy box
      minz = box.z.low + halfz box
  in
    boundingCube box.x.low maxx box.y.low maxy minz box.z.high

bottomNE : BoundingCube ->BoundingCube
bottomNE box = 
  let minx = box.x.low + halfx box
      miny = box.y.low + halfy box
      maxz = box.z.high - halfz box
  in
    boundingCube minx box.x.high miny box.y.high box.z.low maxz

bottomNW : BoundingCube ->BoundingCube
bottomNW box =
  let maxx = box.x.high - halfx box
      miny = box.y.low + halfy box
      maxz = box.z.high - halfz box
  in
    boundingCube box.x.low maxx miny box.y.high box.z.low maxz

bottomSE : BoundingCube ->BoundingCube
bottomSE box =
  let minx = box.x.low + halfx box
      maxy = box.y.high - halfy box
      maxz = box.z.high - halfz box
  in
    boundingCube minx box.x.high box.y.low maxy box.z.low maxz

bottomSW : BoundingCube ->BoundingCube
bottomSW box =    
  let maxx = box.x.high - halfx box
      maxy = box.y.high - halfy box
      maxz = box.z.low - halfz box
  in
    boundingCube box.x.low maxx box.y.low maxy box.z.low maxz


{-| Extend record
-}

type alias Bounded a = {
  boundingBox : BoundingCube
}

{-| In node the order of Octrees is clockwise topNW topNE topSE top SW bottomNW bottomNE  
    bottomSE bottomSW 
    --FOR GOL Int is value 0, for dead, or 1, for alive. A node with 1 is a subdivision
    with at least 1 live cell
-}
type Octree a = Leaf BoundingCube Int | Node BoundingCube Int (Octree a) (Octree a) (Octree a) (Octree a) (Octree a) (Octree a) (Octree a) (Octree a) 

type alias Cell = {
  x : Int,
  y : Int,
  z : Int
}
emptyOctree : BoundingCube -> Int -> Octree a
emptyOctree cube n =
  Leaf cube n


