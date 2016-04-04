module Gol where

import Dict
import Mouse
import Keyboard
import List ((::),map,sum,length)
import List as L
import Time (..)
import Engine (..)
import Math.Vector3 (vec3, Vec3)
import Math.Vector2 (Vec2)
import Graphics.Element as E
import Graphics.Collage as C
import Math.Matrix4 (Mat4,mul,mulAffine,makeRotate,makePerspective,makeLookAt)
import Math.Matrix4
import WebGL as W
import Text (..)
import Signal ((<~),Signal)
import Signal
import Time (Time)

type alias Cell = (Int, Int, Int)
type alias Board = Dict.Dict Cell Bool

-- input
type Action = Flip (Int, Int) | Step

speed : Signal Time
speed = every <| 0.3 * second

paused : Signal Bool
paused = Signal.foldp (\_ isPaused -> not isPaused) False
               (Signal.keepIf (\t -> t) False Keyboard.space)
               
steps : Signal Action
steps = Signal.constant Step |> Signal.sampleOn speed |> Signal.dropWhen paused Step 

flips : Signal Action
flips = Signal.dropRepeats
        <| (\(mx, my) -> Flip (((mx-375) // 100),
                               ((my-375) // 100)))
        <~ (Signal.keepIf (\(mx, my) -> mx-375 <= 375 &&
                                 my-375 <= 375 &&
                                 mx-375 >= -375 && my-375 >= -375)
                   (-1, -1)
        <| Signal.keepWhen Mouse.isDown (-1, -1)
                    Mouse.position)

actions =Signal.merge steps flips                    

angle : Signal Float
angle =
    Signal.foldp (\dt theta -> theta + dt / 15000) 0 (fps 25)

-- helpers
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xs bs =
  case (xs,bs) of
    (_,[])             ->[]
    ([],_)             ->[]
    ((x::xs'),(b::bs'))-> (f x b)::(zipWith f xs' bs')

zip : List a -> List b -> List (a,b)  
zip xs ys = case (xs,ys) of
  (_, []) -> []
  ([], _) -> []  
  ((x::xs'), (y::ys')) -> (x,y)::(zip xs' ys')  


repeated : Int -> a -> List a
repeated n x = if n == 0
                   then []
                   else x :: repeated (n - 1) x

xyCoords : List (List a) -> List (List ((Int,Int),a))
xyCoords rs = let rsWithX = map (\r -> zip [0..length r - 1] r) rs in
                   L.map2 (\r y -> L.map2 (\(x, t) y -> ((x, y), t)) r
                                            <| repeated (length r) y)
                           rsWithX
                           [0..length rsWithX]

   

shortz xs bs =
 L.map2 (\a z -> L.map2 (\((x,y),t) z -> ((x,y,z),t)) a (repeated (length a) z)) xs bs

coords : List (List (List a)) -> List (List (Cell,a))
coords rs = let rsWithXY = map (\r -> xyCoords r) rs
            in
              L.map2 shortz
                rsWithXY
                [0..(length rsWithXY)]


        
uncurry3 : (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) =
  f a b c

-- default parameters
cellx = 1
celly = 1
cellz = 1

xCount = 5
yCount = 5
zCount = 5

boardx = cellx * xCount
boardy = celly * yCount
boardz = cellz * zCount
{--
defaultBoard : Board
defaultBoard = repeated xCount False |> repeated yCount
               |> repeated zCount |> zipCoords |> concat |> Dict.fromList
-}           
    
defaultBoard : Board
defaultBoard = Dict.fromList [((0,0,0),False),((0,0,1),False),((0,0,2),True),((0,1,0),True),((0,1,1),True),((0,1,2),False),((0,2,0),False),((0,2,1),True),((0,2,2),False),((1,0,0),False),((1,1,0),False),((1,1,1),True),((1,0,1),True),((1,2,0),False),((1,2,1),True),((1,2,2),False),((1,0,2),False),((2,0,1),False),((2,0,2),False),((2,0,0),False),((2,1,0),False),((2,2,0),False),((2,2,1),False),((2,2,2),True),((2,1,2),False),((2,1,1),True),((2,1,2),False)]    
    
-- GAME MECHS               

neighborDiffs : List (Int,Int,Int)
neighborDiffs =
    [(1,   0,  0), (1,  1,  0), (1,  0,  1),
     (1,   1,  1), (1, -1,  0), (1,  0, -1),
     (1,  -1, -1), (1, -1,  1), (1,  1, -1),
     (0,   1,  0), (0,  0,  1), (0,  1,  1),
     (0,  -1,  0), (0,  0, -1), (0, -1, -1),
     (0,  -1,  1), (0,  1, -1), (-1, 0,  0),
     (-1, -1,  0), (-1, 0, -1), (-1, 1,  0),
     (-1,  1, -1), (-1, 1,  1), (-1, -1, 0),
     (-1, -1, -1), (-1, 0,  1)]

newState : Bool -> Int -> Bool
newState state n =
         if state
            then if | n < 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      -> False
                    | n == 4 || n == 5 -> True
                    | n > 8 -> False
                    | otherwise -> state

            else if | n == 5 -> True
                    | otherwise -> False
                    
findWithDefault : Bool -> Cell -> Board -> Bool
findWithDefault bool cell board =
  let ans = (Dict.get cell board) in
    case ans of 
      Nothing -> bool
      Just a -> a

countNeighbors : Board -> Int -> Int -> Int -> Int-- Board -> Cell -> Int
countNeighbors board cx cy cz =
  let neighbors = map (\(dx, dy, dz) -> findWithDefault False (cx + dx, cy + dy, cz + dz) board) neighborDiffs 
  in
    sum <| map (\state -> if state then 1 else 0) neighbors
    
flipCell : Cell -> Board -> Board
flipCell cell board = Dict.insert cell
                                  (not <| findWithDefault False cell board)
                                  board    

stepCell : Board -> Cell -> Bool -> Board -> Board
stepCell board cell state board' = 
  let n = uncurry3 (countNeighbors board) cell 
  in 
    Dict.insert cell (newState state n) board'

stepSim : Board -> Board
stepSim board = Dict.foldr (stepCell board) Dict.empty board


stepBoard : Action -> Board-> Board
stepBoard a board = case a of
  Step -> stepSim board
  Flip (x,y) -> flipCell (x,y,0) board
         
---- VIEW

type alias Input = { mouse:Bool, delta:Time }

mat = {
  material | vertexShader <- vertexShader,
             fragmentShader <- fragmentShader}
             
myCube : Float -> Float -> Float -> Renderable
myCube cx cy cz = {
  cube | rotation <- vec3 0 0 0,
         mesh     <- cubeMesh (vec3 cx cy cz) 1 }
         
vp = {
  dimensions = {
    width  = 750.0,
    height = 750.0} }
    
cam= {
  camera | position <- (vec3 0 0 -5),
           rotation <- vec3 0 (pi/8) 0
  }

listCubes : List (Int, Int, Int) -> List Renderable
listCubes xs = 
  case xs of
    [] -> []
    (x, y, z)::xs' -> [myCube (toFloat x) (toFloat y) (toFloat z)]
                      ++ listCubes xs'
                      
myScene : List (Int, Int, Int) -> Scene 
myScene ds = {
  scene | camera <- cam, 
          objects <- listCubes ds,
          viewport <- vp 
          }
         
filters : List (Cell, Bool) -> List (Cell)
filters cells = case cells of
  []               -> []
  (cell,b)::cells' -> if b then [cell] ++(filters cells') else (filters cells') 
  
boardToList : Board -> List (Cell)
boardToList b =
  (Dict.toList b) |> filters  
  
displayBoard : Board -> Scene
displayBoard b =
  (boardToList b) |> myScene       
        
title : E.Element
title = 
  plainText<|
  "The Game of Life in 3D\n"
    
boardState : Signal Board
boardState = Signal.foldp stepBoard defaultBoard actions   
--Signal.map displayBoard boardState
gol = Signal.map render <| Signal.map displayBoard boardState

{- Signal.map perspective angle <| render <| (myScene lives)-}

main = 
     gol 
    

--ROTATES

perspective : Float -> Mat4
perspective angle =
    L.foldr1 mul
        [ perspectiveMatrix
        , cam2
        , makeRotate (3*angle) (vec3 0 1 0)
        ]


perspectiveMatrix : Mat4
perspectiveMatrix =
    makePerspective 45 1 0.01 100


cam2 : Mat4
cam2 =
    makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    
-- SHADERS

--vertexShader : W.Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4 } { vcoord:Vec2 }
vertexShader = """
attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}
"""


--fragmentShader : W.Shader {} { u | texture:W.Texture } { vcoord:Vec2 }
fragmentShader = """
precision mediump float;
uniform sampler2D texture;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(texture, vcoord);
}
"""
         

