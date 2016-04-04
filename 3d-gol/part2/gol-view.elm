module GOLViewer where

import Engine (..)
import Math.Vector3 (vec3, Vec3)
import Math.Vector2 (Vec2)
import Graphics.Element as E
import Graphics.Collage as C
import Math.Matrix4 (..)
import WebGL (..)
import Text (..)
import Mouse
import Signal
import Time (Time)

--import Life (..)
{-
type alias Scene = {
  camera    : Camera,
  objects   : List Renderable,
  light     : Light,
  viewport  : Viewport
}

{--draw  Cell : Cell -> Int -> Int -> Int-> Renderable
drawCell c x y z = {
  cube | position <- vec3 x y z
       | rotation <- vec3 0 0 0 
       | scale <- vec3 1.0 1.0 1.0 }
-      
makeBoard : Board -> List Renderable
makeBoard b = []
-}-}
-- INPUTS
type alias Input = { mouse:Bool, delta:Time }

myCube = {
  cube | position <- vec3 0 0 0,
         rotation <- vec3 0 0 0,
         scale    <- vec3 1 1 1 }
         
vp = {
  dimensions = {
    width  = 500.0,
    height = 500.0} }
    
cam = {
  camera | position <- vec3 0 0 -50}

listCubes : List (Int, Int, Int) -> List Renderable
listCubes xs = 
  case xs of
    [] -> []
    (x, y, z)::xs' -> [{myCube | position <- vec3 (toFloat x) (toFloat y) (toFloat z)}]
                      ++ listCubes xs'
                      
lives = [(0,0,0),(0,1,0),(1,0,0),(0,20,0),(0,5,5),(5,5,5), (1,1,1),(4,18,4), (-4,-18,-4)
        , (-18,-18,-18), (18,18,18),(18,18,1),(18,17,1)]

myScene : List (Int, Int, Int) -> Scene 
myScene ds = {
  scene | camera <- cam, 
          objects <- listCubes ds,
          viewport <- vp  
          }
        
title : E.Element
title = 
  plainText<|
    "The Game of Life in 3D\n"

gol = render (myScene lives)

main = 
  E.flow E.down 
    [E.container 500 100 E.midTop title,
     gol 
    ]


-- SHADERS

vertexShader : Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]


fragmentShader : Shader {} { u | texture:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D texture;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(texture, vcoord);
}

|]

