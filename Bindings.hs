module Bindings (
	makeGameObjects, 
	paddleCommand, 
	Side(PLeft, PRight), 
	MoveDirection(DNone, DUp, DDown), 
	MoveDirections,
	GameState(GameState), 
	GameObject(Ball,Paddle) , 
	paddleSpeed,
	upVertex,
	downVertex, 
	gameObjects, 
	moveDirections) where 

import Graphics.UI.GLUT
import Data.IORef
--import Display 
import Games2
import GHC.Float

-- Types --

-- Either Movement up or down 
data MoveDirection = DNone | DUp | DDown deriving (Eq)

-- Type for (leftPaddle, rightPaddle) of vertical paddle movement 
type MoveDirections = (MoveDirection, MoveDirection)

-- GameObjects are either Ball position velocity radius or Paddle corner1 corner2 corner3 corner4
data GameObject = Ball (Vertex3 GLfloat) (Vertex3 GLfloat) GLfloat | 
    Paddle (Vertex3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat)

-- GameState is a list of game objects and movement of paddle controls 
data GameState = GameState { gameObjects :: [GameObject], moveDirections :: MoveDirections }


-- A GameAction has a move type of MoveDirection, a state type of 
type GameAction = Action MoveDirection 


-- Paddle sides 
data Side = PLeft | PRight

-- Constants -- 

paddleSpeed = double2Float 0.05

upVertex = Vertex3 0 paddleSpeed 0

downVertex = Vertex3 0 (-paddleSpeed) 0
 



makeGameObjects :: GameState
makeGameObjects = GameState [myPaddle PLeft 0.0, myPaddle PRight 0.0] (DNone, DNone)

myPaddle :: Side -> GLfloat -> GameObject 
myPaddle PLeft y = Paddle 
    (Vertex3 (-0.95)  y  0)
    (Vertex3 (-0.95) (y+0.2) 0) 
    (Vertex3 (-0.9) (y+0.2) 0)
    (Vertex3 (-0.9) y 0)
myPaddle PRight y = Paddle 
    (Vertex3 0.95  y  0)
    (Vertex3 0.95 (y+0.2) 0) 
    (Vertex3 0.9 (y+0.2) 0)
    (Vertex3 0.9 y 0)


paddleCommand :: MoveDirection -> Side -> GameState -> GameState 
paddleCommand d PLeft (GameState [leftPaddle, rightPaddle] (dirLeft, dirRight)) =
    GameState [leftPaddle, rightPaddle] (d, dirRight) 
paddleCommand d PRight (GameState [leftPaddle, rightPaddle] (dirLeft, dirRight)) = 
    GameState [leftPaddle, rightPaddle] (dirLeft, d)